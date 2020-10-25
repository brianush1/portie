import { AST } from "./parser";

class Environment {

	variables = new Map<string, string>();

	constructor(
		public parent: Environment | undefined,
		public prefix: string,
		public globalDecls: boolean,
		public attributes: { [x: string]: boolean } = {},
	) { }

	declare(name: string) {
		this.variables.set(name, `${this.prefix}${name}`);
		return `${this.prefix}${name}`;
	}

	get(name: string): string {
		const result = this.variables.get(name) ?? this.parent?.get(name);
		if (!result) {
			throw new Error("internal compiler error");
		}
		return result;
	}

}

function indent(x: string) {
	return x.split("\n").map(x => `\t${x}`).join("\n");
}

class Compiler {

	constructor(public ast: AST.File) { }

	genv = new Environment(undefined, "g_", true, {
		private: false,
	});
	env = this.genv;

	publicDecls: string[] = [];

	newEnv() {
		this.env = new Environment(this.env, "c_", false);
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
					(condition.inverted ? "not " : "") + `s.toBool(${this.compile(condition.value)})`
				)}`;
		}
		else {
			return blockPattern.replace("$",
				(condition.inverted ? "not " : "") + `s.toBool(${this.compile(condition.value)})`
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
		program.push("local s = require(\"out.__runtime\")");
		for (const impor of node.imports) {
			program.push(`local ${impor.symbols.map(x => this.env.declare(x))
				.join(", ")} = s.import(${[impor.module, ...impor.symbols]
				.map(x => `"${x}"`).join(", ")})`);
		}
		program.push("local exports = {}");
		const globals = [];
		for (const decl of node.body) {
			if (decl.kind !== "scoped-attribute") {
				globals.push(this.env.declare(decl.name));
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

				program.push(this.compile(decl));
			}
		}
		for (const publicDecl of this.publicDecls) {
			program.push(`exports.${publicDecl} = ${this.env.get(publicDecl)}`);
			if (publicDecl === "main") {
				program.push(`${this.env.get(publicDecl)}()`);
			}
		}
		program.push("return exports");
		return program.join(";\n");
	}

	compileFuncCall(node: AST.FuncCall) {
		return `s.call(${[node.func, ...node.args].map(x => this.compile(x)).join(", ")})`;
	}

	compileNewCall(node: AST.NewCall) {
		return `s.new({${
			node.fields.flatMap(x => [`"${x.name}"`, this.compile(x.value)]).join(", ")
		}}, ${[node.classObj, ...node.args].map(x => this.compile(x)).join(", ")})`;
	}

	compilePipeCall(node: AST.PipeCall) {
		return `s.call(${[this.genv.get(node.func),
			...[node.base, ...node.args].map(x => this.compile(x))].join(", ")})`;
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

	compileAssign(node: AST.Assign) {
		if (node.lvalue.kind === "index") {
			return `s.newindex(${[node.lvalue.base,
				...node.lvalue.args, node.rvalue].map(x => this.compile(x)).join(", ")})`;
		}
		else {
			return `${this.env.get(node.lvalue.name)} = ${this.compile(node.rvalue)}`;
		}
	}

	compileFuncDecl(node: AST.FuncDecl) {
		this.env.declare(node.name);
		const local = this.env.globalDecls ? "" : "local ";
		this.newEnv();
		const result = `${local}function ${this.env.get(node.name)}(${
			node.params.map(x => this.env.declare(x.name)).join(", ")})\n`
			+ indent(this.block(node.body)) + `\nend`;
		this.exitEnv();
		return result;
	}

	compileVarDecl(node: AST.VarDecl) {
		return `${this.env.globalDecls ? "" : "local "}${this.env.declare(node.name)} = ${this.compile(node.value)}`;
	}

	compileClassDecl(node: AST.ClassDecl) {
		const name = this.env.declare(node.name);
		this.env = new Environment(this.env, `${name}.decl.`, true, {
			private: false,
		});
		const body = node.body.map(x => {
			if (x.kind === "var-decl") {
				return `${name}.fields.${x.name} = function(this) return ${this.compile(x.value)} end`;
			}
			else if (x.kind === "empty-var-decl") {
				return `${name}.fields.${x.name} = "empty"`;
			}
			else if (x.kind === "func-decl") {
				this.env.declare(x.name);
				this.newEnv();
				const result = `function ${this.env.get(x.name)}(${
					["this", ...x.params.map(x => this.env.declare(x.name))].join(", ")})\n`
					+ indent(this.block(x.body)) + `\nend`;
				this.exitEnv();
				return result;
			}
			else if (x.kind === "scoped-attribute") {
				return this.compile(x);
			}
			else { // TODO: classes inside classes?
				throw 0;
			}
		}).join(";\n");
		this.exitEnv();
		return `${this.env.globalDecls ? "" : "local "}${name} = s.class(${
			node.base ? this.compile(node.base) : ""}) do\n`
			+ indent(body) + `\nend`;
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
		return "\"" + [...new TextEncoder().encode(node.value)].map(x => {
			if (x >= 32 && x <= 126) {
				return String.fromCodePoint(x);
			}
			else {
				return `\\${x.toString().padStart(3, "0")}`;
			}
		}).join("") + "\"";
	}

	compileNumLiteral(node: AST.NumLiteral) {
		return `${node.value}`; // TODO: better precision
	}

	compileNilLiteral(node: AST.NilLiteral) {
		return "nil";
	}

	compileThisLiteral(node: AST.ThisLiteral) {
		return "this";
	}

	compileSuperLiteral(node: AST.SuperLiteral) {
		return "s.super(this)";
	}

	compileVarAccess(node: AST.VarAccess) {
		return this.env.get(node.name);
	}

	compileTableLiteral(node: AST.TableLiteral) {
		return "s.table {\n" + indent(node.fields.map(x => `[${this.compile(x.key)}] = ${this.compile(x.value)};`).join("\n")) + "\n}";
	}

	compileArrayLiteral(node: AST.ArrayLiteral) {
		return `s.array(${node.values.length}, {${node.values.map(x => this.compile(x)).join(", ")}})`;
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
