import { Diagnostic } from "./diagnostic";
import { AST } from "./parser";

class Environment {

}

type Type = {};

interface TypecheckerResult {
	exportedTypes: { [x: string]: Type };
	dependsOn: string[];
}

class Typechecker {

	result: TypecheckerResult = {
		exportedTypes: {},
		dependsOn: [],
	};

	constructor(public ast: AST.File, public diagnostics: Diagnostic[]) { }

	check(node: { kind: string }) {
		((this as any)[`check${node.kind.substr(0, 1).toUpperCase()
			}${node.kind.substr(1)}`] as (node: any) => void)(node);
	}

	checkFile(file: AST.File) {
		for (const decl of file.body) {
			
		}
	}

}

export function typecheck(ast: AST.File, diagnostics: Diagnostic[]): TypecheckerResult {
	const checker = new Typechecker(ast, diagnostics);
	checker.check(ast);
	return checker.result;
}
