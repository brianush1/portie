import { exec, spawn } from "child_process";
import { mkdirSync, readdirSync, readFile, readFileSync, rmdirSync, writeFileSync } from "fs";
import { compile } from "./compiler";
import { Diagnostic } from "./diagnostic";
import { Lexer } from "./lexer";
import { Parser } from "./parser";
import { report } from "./reporter";
import { typecheck } from "./typechecker";

const RUNTIME_SRC = readFileSync(`${__dirname}/../src/runtime.lua`, "utf-8");

const args = process.argv.slice(2);

interface Project {
	name: string;
	["module-root"]: string;
	dependencies: { type: "local", path: string }[];
}

function loadProject(root: string = "."): Project {
	return JSON.parse(readFileSync(`${root}/project.json`, "utf-8"));
}

export const files: {
	src: string,
	module: string[],
}[] = [];

function buildProject(project: Project): boolean {
	try { mkdirSync("./out"); } catch (e) {}
	writeFileSync("./out/__runtime.lua", RUNTIME_SRC, "utf-8");
	writeFileSync(`./${project.name}`,
		`#!/bin/bash\n\nlua5.1 out/${project["module-root"]}/index.lua "$@"`, {
		encoding: "utf-8",
		mode: "777",
	});

	let success = true;

	function buildFolder(root: string, moduleRoot: string, path: string[]) {
		const children = readdirSync(`${[root, ...path].join("/")}`, { withFileTypes: true });
		try { mkdirSync(`./out/${[moduleRoot, ...path].join("/")}`); } catch (e) {}
		for (const child of children) {
			// TODO: require files to match a certain naming pattern
			if (child.isDirectory()) {
				buildFolder(root, moduleRoot, [...path, child.name]);
			}
			else if (child.isFile() && child.name.endsWith(".pie")) {
				const src = readFileSync(`${[root, ...path, child.name].join("/")}`, "utf-8");
				const diagnostics: Diagnostic[] = [];
				const fileId = files.length;
				files.push({
					src,
					module: [moduleRoot, ...path, child.name.substr(0, child.name.length - 4)],
				});
				const parse = new Parser(fileId, src, diagnostics);
				const ast = parse.parse();
				let reported = report(diagnostics);
				if (reported !== "errors") {
					const diagnostics: Diagnostic[] = [];
					typecheck(ast, diagnostics);
					reported = report(diagnostics);
				}
				if (reported !== "errors") {
					writeFileSync(`./out/${[moduleRoot, ...path, child.name.substr(0, child.name.length - 4)]
						.join("/")}.lua`, compile(ast), "utf-8");
				}
				else {
					success = false;
				}
			}
		}
	}

	buildFolder("./src", project["module-root"], []);
	for (const dep of project.dependencies) {
		buildFolder(`${dep.path}/src`, loadProject(dep.path)["module-root"], []);
	}

	return success;
}

if (args[0] === "build") {
	const project = loadProject();
	const success = buildProject(project);
	if (success) {
		console.error("Build succeeded.");
	}
	else {
		console.error("Build failed.");
	}
}
else if (args[0] === "run") {
	const project = loadProject();
	const success = buildProject(project);
	if (success) {
		spawn(`./${project.name}`, [], { stdio: "inherit" });
	}
	else {
		console.error("Build failed.");
	}
}
else {
	console.error(`Unknown command '${args[0]}'; see 'piec help'`);
}