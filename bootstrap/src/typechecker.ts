import { Diagnostic, Span } from "./diagnostic";
import { AST } from "./parser";

type VarInfo = { isConst: boolean };

class Environment {

	variables = new Map<string, VarInfo>();

	constructor(
		public checker: Typechecker,
		public parent?: Environment,
		public attributes: { [x: string]: boolean } = {},
	) { }

	declare(span: Span, name: string, isConst: boolean) {
		if (this.variables.has(name)) {
			this.checker.diagnostics.push({
				kind: "error",
				message: `redeclaration of '${name}'`,
				spans: [span],
			});
		}
		else {
			this.variables.set(name, { isConst });
		}
	}

	get(name: string): VarInfo | undefined {
		return this.variables.get(name) ?? this.parent?.get(name);
	}

}

interface TypecheckerResult {
	publicTypes: { [x: string]: VarInfo };
	dependsOn: string[];
}

class Typechecker {

	result: TypecheckerResult = {
		publicTypes: {},
		dependsOn: [],
	};

	constructor(public ast: AST.File, public diagnostics: Diagnostic[]) { }

	genv = new Environment(this, undefined, {
		private: false,
	});
	env = this.genv;

	newEnv() {
		this.env = new Environment(this, this.env);
	}

	exitEnv() {
		const parent = this.env.parent;
		if (parent === undefined) {
			throw new Error("internal compiler error: env cannot be undefined");
		}
		this.env = parent;
	}

	// Checker methods:

	check(node: { kind: string }) {
		const funcName = `check${("-" + node.kind).replace(/\-\w/g,
			x => x.substr(1).toUpperCase())}`;
		const func = ((this as any)[funcName] as ((node: any) => void) | undefined);
		if (!func) {
			throw new Error(`function ${funcName} not implemented`);
		}
		func.apply(this, [node]);
	}

	checkCondition(node: AST.Condition) {
		if (node.decl) {
			this.check(node.decl);
		}
		this.check(node.value);
	}

	checkFile(node: AST.File) {
		for (const impor of node.imports) {
			for (const symbol of impor.symbols) {
				this.env.declare(impor.span, symbol, true);
			}
		}
		for (const decl of node.body) {
			if (decl.kind === "scoped-attribute") {
				this.check(decl);
			}
			else {
				if (decl.kind === "func-decl") {
					this.result.publicTypes[decl.name] = { isConst: true };
					this.env.declare(decl.span, decl.name, true);
				}
				else if (decl.kind === "var-decl") {
					this.result.publicTypes[decl.name] = { isConst: decl.isConst };
					this.env.declare(decl.span, decl.name, decl.isConst);
				}
				else if (decl.kind === "class-decl") {
					this.result.publicTypes[decl.name] = { isConst: true };
					this.env.declare(decl.span, decl.name, true);
				}
				else {
					throw 0;
				}
			}
		}
		for (const decl of node.body) {
			if (decl.kind === "scoped-attribute") {
				continue;
			}
			if (decl.name === "this") {
				this.diagnostics.push({
					kind: "error",
					message: "'this' cannot be used as a variable name",
					spans: [decl.span],
				});
			}
			if (decl.kind === "func-decl") {
				// don't forget about checkFuncDecl
				this.newEnv();
				for (const param of decl.params) {
					this.env.declare(decl.span, param.name, true);
				}
				for (const stat of decl.body) {
					this.check(stat);
				}
				this.exitEnv();
			}
			else if (decl.kind === "var-decl") {
				// don't forget about checkVarDecl
				this.check(decl.value);
			}
			else if (decl.kind === "class-decl") {
				// don't forget about checkClassDecl
				if (decl.base) {
					this.check(decl.base);
				}
				this.env = new Environment(this, this.env, {
					private: false,
				});
				this.env.declare(decl.span, "this", true);
				for (const inner of decl.body) {
					if (inner.kind === "func-decl" && inner.name === "this") {
						this.newEnv();
						for (const param of inner.params) {
							this.env.declare(inner.span, param.name, true);
						}
						for (const stat of inner.body) {
							this.check(stat);
						}
						this.exitEnv();
					}
					else if (inner.kind === "empty-var-decl") {
						continue;
					}
					else {
						this.check(inner);
					}
				}
				this.exitEnv();
			}
			else {
				throw 0;
			}
		}
	}

	checkFuncCall(node: AST.FuncCall) {
		this.check(node.func);
		for (const arg of node.args) {
			this.check(arg);
		}
	}

	checkNewCall(node: AST.NewCall) {
		this.check(node.classObj);
		for (const arg of node.args) {
			this.check(arg);
		}
		for (const field of node.fields) {
			this.check(field.value);
		}
	}

	checkPipeCall(node: AST.PipeCall) {
		this.check(node.base);
		if (!this.genv.get(node.func)) {
			this.diagnostics.push({
				spans: [node.span],
				kind: "error",
				message: `use of undeclared variable '${node.func}'`,
			});
		}
		for (const arg of node.args) {
			this.check(arg);
		}
	}

	checkIf(node: AST.If) {
		this.newEnv();
		this.checkCondition(node.cond);
		for (const stat of node.body) {
			this.check(stat);
		}
		for (const stat of node.elseBody) {
			this.check(stat);
		}
		this.exitEnv();
	}

	checkWhile(node: AST.While) {
		this.newEnv();
		this.checkCondition(node.cond);
		for (const stat of node.body) {
			this.check(stat);
		}
		this.exitEnv();
	}

	checkFor(node: AST.For) {
		this.newEnv();
		this.check(node.value);
		this.env.declare(node.span, node.name, true);
		for (const stat of node.body) {
			this.check(stat);
		}
		this.exitEnv();
	}

	checkReturn(node: AST.Return) {
		if (node.value) {
			this.check(node.value);
		}
	}

	checkInlineLua(node: AST.InlineLua) {}

	checkAssign(node: AST.Assign) {
		this.check(node.lvalue);
		this.check(node.rvalue);

		if (node.lvalue.kind === "var-access") {
			const info = this.env.get(node.lvalue.name);
			if (!info) {
				this.diagnostics.push({
					spans: [node.span],
					kind: "error",
					message: `use of undeclared variable '${node.lvalue.name}'`,
				});
			}
			else if (info.isConst) {
				this.diagnostics.push({
					spans: [node.span],
					kind: "error",
					message: `cannot reassign const variable '${node.lvalue.name}'`,
				});
			}
		}
	}

	checkFuncDecl(node: AST.FuncDecl) {
		// don't forget about checkFile
		this.env.declare(node.span, node.name, true);
		this.newEnv();
		for (const param of node.params) {
			this.env.declare(node.span, param.name, true);
		}
		for (const stat of node.body) {
			this.check(stat);
		}
		this.exitEnv();
	}

	checkVarDecl(node: AST.VarDecl) {
		// don't forget about checkFile
		this.check(node.value);
		this.env.declare(node.span, node.name, node.isConst);
	}

	checkClassDecl(node: AST.ClassDecl) {
		// don't forget about checkFile
		if (node.base) {
			this.check(node.base);
		}
		this.env.declare(node.span, node.name, true);
		this.env = new Environment(this, this.env, {
			private: false,
		});
		this.env.declare(node.span, "this", true);
		for (const inner of node.body) {
			if (inner.kind === "func-decl" && inner.name === "this") {
				this.newEnv();
				for (const param of inner.params) {
					this.env.declare(inner.span, param.name, true);
				}
				for (const stat of inner.body) {
					this.check(stat);
				}
				this.exitEnv();
			}
			else if (inner.kind === "empty-var-decl") {
				continue;
			}
			else {
				this.check(inner);
			}
		}
		this.exitEnv();
	}

	checkScopedAttribute(node: AST.ScopedAttribute) {
		if (node.value !== undefined) {
			this.diagnostics.push({
				spans: [node.value.span],
				kind: "error",
				message: "attribute values are not supported in the bootstrap compiler",
			});
		}

		if (node.which in this.env.attributes) {
			this.env.attributes[node.which] = true;
		}
		else {
			this.diagnostics.push({
				spans: [node.span],
				kind: "error",
				message: `attribute '${node.which}' not valid in scope`,
			});
		}
	}

	checkStrLiteral(node: AST.StrLiteral) {}

	checkNumLiteral(node: AST.NumLiteral) {}

	checkNilLiteral(node: AST.NilLiteral) {}

	checkBoolLiteral(node: AST.BoolLiteral) {}

	checkThisLiteral(node: AST.ThisLiteral) {
		if (!this.env.get("this")) {
			this.diagnostics.push({
				spans: [node.span],
				kind: "error",
				message: `use of 'this' outside of class`,
			});
		}
	}

	checkSuperLiteral(node: AST.SuperLiteral) {
		if (!this.env.get("this")) {
			this.diagnostics.push({
				spans: [node.span],
				kind: "error",
				message: `use of 'super' outside of class`,
			});
		}
	}

	checkVarAccess(node: AST.VarAccess) {
		if (!this.env.get(node.name)) {
			this.diagnostics.push({
				spans: [node.span],
				kind: "error",
				message: `use of undeclared variable '${node.name}'`,
			});
		}
	}

	checkTableLiteral(node: AST.TableLiteral) {
		for (const field of node.fields) {
			this.check(field.value);
		}
	}

	checkArrayLiteral(node: AST.ArrayLiteral) {
		for (const value of node.values) {
			this.check(value);
		}
	}

	checkFuncLiteral(node: AST.FuncLiteral) {
		this.newEnv();
		for (const param of node.params) {
			this.env.declare(node.span, param.name, true);
		}
		for (const stat of node.body) {
			this.check(stat);
		}
		this.exitEnv();
	}

	checkBinOp(node: AST.BinOp) {
		this.check(node.lhs);
		this.check(node.rhs);
	}

	checkUnOp(node: AST.UnOp) {
		this.check(node.value);
	}

	checkIndex(node: AST.Index) {
		this.check(node.base);
		for (const arg of node.args) {
			this.check(arg);
		}
	}

	checkIfExpr(node: AST.IfExpr) {
		this.newEnv();
		this.checkCondition(node.cond);
		this.check(node.value);
		this.check(node.elseValue);
		this.exitEnv();
	}

}

export function typecheck(ast: AST.File, diagnostics: Diagnostic[]): TypecheckerResult {
	const checker = new Typechecker(ast, diagnostics);
	checker.check(ast);
	return checker.result;
}
