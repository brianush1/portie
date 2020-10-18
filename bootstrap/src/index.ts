import { readFileSync } from "fs";
import { compile } from "./compiler";
import { Diagnostic } from "./diagnostic";
import { Lexer } from "./lexer";
import { Parser } from "./parser";
import { report } from "./reporter";
import { typecheck } from "./typechecker";

const src = readFileSync("../tests/1-simple.pie", "utf-8");
const diagnostics: Diagnostic[] = [];
const parse = new Parser(0, src, diagnostics);
const ast = parse.parse();
let reported = report(diagnostics);
if (reported !== "errors") {
	const diagnostics: Diagnostic[] = [];
	typecheck(ast, diagnostics);
	reported = report(diagnostics);
}
if (reported !== "errors") {
	console.log(compile(ast));
}
