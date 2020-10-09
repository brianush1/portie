import { readFileSync } from "fs";
import { Diagnostic } from "./diagnostic";
import { Lexer } from "./lexer";
import { Parser } from "./parser";
import { report } from "./reporter";
import { typecheck } from "./typechecker";

const src = readFileSync("../tests/1-simple.pie", "utf-8");
const diagnostics: Diagnostic[] = [];
const parse = new Parser(0, src, diagnostics);
const ast = parse.parse();
const reported = report(diagnostics);
if (reported !== "errors") {
	const diagnostics: Diagnostic[] = [];
	const { exportedTypes } = typecheck(ast, diagnostics);
	const reported = report(diagnostics);
	if (reported !== "errors") {
		console.log(JSON.stringify(ast, (key, value) => {
			if (key === "span") {
				return undefined;
			}
			else {
				return value;
			}
		}, 4));
	}
}
