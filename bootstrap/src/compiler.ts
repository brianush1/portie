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
			program.push(`exports["${publicDecl}"] = ${this.env.get(publicDecl)}`);
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

	compileFor(node: AST.For) {
		this.newEnv();
		const varName = this.env.declare(node.name);
		let result = `do\n\tlocal range, elem = ${this.compile(node.value)}\n`
			+ `\twhile not s.call(s.index(range, "empty")) do\n`
			+ `\t\telem = s.call(s.index(range, "front"))\n`
			+ `\t\trange = s.call(s.index(range, "popFront"))\n`
			+ `\t\tlocal ${varName} = elem;\n`;
		result += indent(indent(this.block(node.body)));
		result += "\n\tend\nend";
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

	compileBreak(node: AST.Break) {
		return `break`;
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
		const local = this.env.globalDecls ? "" : `local ${this.env.get(node.name)} `;
		const result = `${local}${this.env.get(node.name)} = ${this.compileFuncLiteral({
			...node,
			kind: "func-literal",
		})}`;
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
				return `${name}.fields["${x.name}"] = function(this) return ${this.compile(x.value)} end`;
			}
			else if (x.kind === "empty-var-decl") {
				return `${name}.fields["${x.name}"] = "empty"`;
			}
			else if (x.kind === "func-decl") {
				this.env.declare(x.name);
				this.newEnv();
				// const result = `function ${this.env.get(x.name)}(${
				// 	["this", ...x.params.map(x => this.env.declare(x.name))].join(", ")})\n`
				// 	+ indent(this.block(x.body)) + `\nend`;
				const result = `${this.env.get(x.name)} = ${this.compileFuncLiteral({
					...x,
					kind: "func-literal",
				}, ["this"])}`;
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
			node.base ? `"${node.name}", ${this.compile(node.base)}` : `"${node.name}"`}) do\n`
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

	compileBoolLiteral(node: AST.BoolLiteral) {
		return node.value ? "true" : "false";
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

	compileFuncLiteral(node: AST.FuncLiteral, extraParams: string[] = []) {
		this.newEnv();
		let numRequired = node.params.filter(x => x.defaultValue === undefined && !("rest" in x)).length;
		node.params.forEach(x => this.env.declare(x.name));
		const params = [...extraParams,
			...node.params.slice(0, numRequired).map(x => this.env.get(x.name)), "..."].join(", ");
		const body = node.body.map(x => this.compile(x));
		const preBody = [];
		preBody.push("local paramsGiven = select(\"#\", ...)");
		if (numRequired < node.params.length) {
			preBody.push(`local ${node.params.slice(numRequired).map(x => this.env.get(x.name)).join(", ")}`);
		}
		for (let i = numRequired, j = 0; i < node.params.length; ++i, ++j) {
			const param = node.params[i];
			const name = this.env.get(param.name);
			if ("rest" in param) {
				preBody.push(`${name} = s.rest(${node.params.length - numRequired - 1}, ...)`);
			}
			else {
				if (!param.defaultValue) {
					throw 0;
				}
				preBody.push(`if paramsGiven <= ${j} then\n`
					+ `\t${name} = ${this.compile(param.defaultValue)}`);
				preBody.push(`else\n`
					+ `\t${name} = select(${j + 1}, ...)`);
				preBody.push(`end`);
			}
		}
		body.splice(0, 0, ...preBody);
		const result = `function(${params})\n`
			+ indent(body.join(";\n")) + `\nend`;
		this.exitEnv();
		return result;
	}

	compileBinOp(node: AST.BinOp) {
		// TODO: use s.toBool?
		if (node.op === "&&") {
			return `(${this.compile(node.lhs)} and ${this.compile(node.rhs)})`;
		}
		else if (node.op === "||") {
			return `(${this.compile(node.lhs)} or ${this.compile(node.rhs)})`;
		}
		else {
			return `s.applyBinOp("${node.op}", ${this.compile(node.lhs)}, ${this.compile(node.rhs)})`;
		}
	}

	compileUnOp(node: AST.UnOp) {
		return `s.applyUnOp("${node.op}", ${this.compile(node.value)})`
	}

	compileIndex(node: AST.Index) {
		return `s.index(${[node.base, ...node.args].map(x => this.compile(x)).join(", ")})`;
	}

	compileIfExpr(node: AST.IfExpr) {
		this.newEnv();
		let result = this.condition(node.cond, "if $ then\n");
		result += indent("return " + this.compile(node.value));
		result += "\nelse\n";
		result += indent("return " + this.compile(node.elseValue));
		result += "\nend" + this.closeCondition(node.cond);
		this.exitEnv();
		return `(function()\n${indent(result)}\nend)()`;
	}

}

export function compile(ast: AST.File): string {
	const compiler = new Compiler(ast);
	return compiler.compile(ast);
}
