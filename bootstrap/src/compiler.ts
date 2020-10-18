import { AST } from "./parser";

class Environment {

	constructor(
		public parent?: Environment,
		public attributes: { [x: string]: boolean } = {},
	) { }

}

function indent(x: string) {
	return x.split("\n").map(x => `\t${x}`).join("\n");
}

class Compiler {

	constructor(public ast: AST.File) { }

	env = new Environment(undefined, {
		private: false,
	});

	publicDecls: string[] = [];

	newEnv() {
		this.env = new Environment(this.env);
	}

	exitEnv() {
		const parent = this.env.parent;
		if (parent === undefined) {
			throw new Error("internal compiler error: env cannot be undefined");
		}
		this.env = parent;
	}

	// Miscellaneous:

	block(stats: AST.Stat[]) {
		return stats.map(x => this.compile(x)).join(";\n");
	}

	condition(condition: AST.Condition, blockPattern: string) {
		if (condition.decl) {
			return `do ${this.compile(condition.decl)} `
				+ `${blockPattern.replace("$",
					(condition.inverted ? "not " : "") + `(${this.compile(condition.value)})`
				)}`;
		}
		else {
			return blockPattern.replace("$",
				(condition.inverted ? "not " : "") + `(${this.compile(condition.value)})`
			);
		}
	}

	closeCondition(condition: AST.Condition) {
		if (condition.decl) {
			return ` end`;
		}
		else {
			return ``;
		}
	}

	// Checker methods:

	compile(node: { kind: string }): string {
		const funcName = `compile${("-" + node.kind).replace(/\-\w/g,
			x => x.substr(1).toUpperCase())}`;
		const func = ((this as any)[funcName] as ((node: any) => string) | undefined);
		if (!func) {
			throw new Error(`function ${funcName} not implemented`);
		}
		return func.apply(this, [node]);
	}

	compileFile(node: AST.File) {
		const program = [];
		program.push("local s = require(\"out.runtime\")");
		for (const impor of node.imports) {
			program.push(`${impor.symbols.map(x => `c_${x}`)
				.join(", ")} = s.import(${[impor.module, ...impor.symbols]
				.map(x => `"${x}"`).join(", ")})`);
		}
		program.push("local exports = {}");
		const globals = [];
		for (const decl of node.body) {
			if (decl.kind !== "scoped-attribute") {
				globals.push(`c_${decl.name}`);
			}
		}
		program.push(`local ${globals.join(", ")}`);
		for (const decl of node.body) {
			if (decl.kind === "scoped-attribute") {
				this.compile(decl);
			}
			else {
				if (this.env.attributes["private"] === false) {
					this.publicDecls.push(decl.name);
				}

				if (decl.kind === "func-decl") {
					// don't forget about compileFuncDecl
					this.newEnv();
					const result = `function c_${decl.name}(${decl.params.map(x => `c_${x.name}`).join(", ")})\n`
						+ indent(this.block(decl.body)) + `\nend`;
					this.exitEnv();
					program.push(result);
				}
				else if (decl.kind === "var-decl") {
					// don't forget about compileVarDecl
					program.push(`c_${decl.name} = ${this.compile(decl.value)}`);
				}
			}
		}
		for (const publicDecl of this.publicDecls) {
			program.push(`exports.${publicDecl} = c_${publicDecl}`);
		}
		program.push("return exports");
		return program.join(";\n");
	}

	compileFuncCall(node: AST.FuncCall) {
		return `s.call(${[node.func, ...node.args].map(x => this.compile(x)).join(", ")})`;
	}

	compileIf(node: AST.If) {
		this.newEnv();
		let result = this.condition(node.cond, "if $ then\n");
		result += indent(this.block(node.body));
		result += "\nelse\n";
		result += indent(this.block(node.elseBody));
		result += "\nend" + this.closeCondition(node.cond);
		this.exitEnv();
		return result;
	}

	compileWhile(node: AST.While) {
		this.newEnv();
		let result = this.condition(node.cond, "while $ do\n");
		result += indent(this.block(node.body));
		result += "\nend" + this.closeCondition(node.cond);
		this.exitEnv();
		return result;
	}

	compileReturn(node: AST.Return) {
		if (node.value) {
			return `return ${this.compile(node.value)}`;
		}
		else {
			return `return`;
		}
	}

	compileInlineLua(node: AST.InlineLua) {
		return node.value;
	}

	compileFuncDecl(node: AST.FuncDecl) {
		// don't forget about compileFile
		this.newEnv();
		const result = `local function c_${node.name}(${node.params.map(x => `c_${x.name}`).join(", ")})\n`
			+ indent(this.block(node.body)) + `\nend`;
		this.exitEnv();
		return result;
	}

	compileVarDecl(node: AST.VarDecl) {
		// don't forget about compileFile
		return `local c_${node.name} = ${this.compile(node.value)}`;
	}

	compileScopedAttribute(node: AST.ScopedAttribute) {
		if (node.value !== undefined) {
			throw 0;
		}

		if (node.which in this.env.attributes) {
			this.env.attributes[node.which] = true;
		}
		else {
			throw 0;
		}

		return "_=_";
	}

	compileStrLiteral(node: AST.StrLiteral) {
		return "s.string\"" + [...new TextEncoder().encode(node.value)].map(x => {
			if (x >= 32 && x <= 126) {
				return String.fromCodePoint(x);
			}
			else {
				return `\\${x.toString().padStart(3, "0")}`;
			}
		}).join("") + "\"";
	}

	compileNumLiteral(node: AST.NumLiteral) {
		return node.value.toString(); // TODO: better
	}

	compileNilLiteral(node: AST.NilLiteral) {
		return "nil";
	}

	compileVarAccess(node: AST.VarAccess) {
		return `c_${node.name}`;
	}

	compileTableLiteral(node: AST.TableLiteral) {
		return "{\n" + indent(node.fields.map(x => `[${this.compile(x.key)}] = ${this.compile(x.value)};`).join("\n")) + "\n}";
	}

	compileBinOp(node: AST.BinOp) {
		return `s.applyBinOp("${node.op}", ${this.compile(node.lhs)}, ${this.compile(node.rhs)})`
	}

	compileUnOp(node: AST.UnOp) {
		return `s.applyUnOp("${node.op}", ${this.compile(node.value)})`
	}

	compileIndex(node: AST.Index) {
		return `s.index(${[node.base, ...node.args].map(x => this.compile(x)).join(", ")})`;
	}

}

export function compile(ast: AST.File): string {
	const compiler = new Compiler(ast);
	return compiler.compile(ast);
}
