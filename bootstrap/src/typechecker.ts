import { Diagnostic, Span } from "./diagnostic";
import { AST } from "./parser";

export abstract class TypeInfo {

	/** This should not be called directly; use Types.isSupersetOf */
	abstract isSupersetOf(other: TypeInfo): boolean;

	/** This should not be called directly; use Types.isSubsetOf */
	isSubsetOf(other: TypeInfo): boolean {
		return other.isSupersetOf(this);
	}

	/** This should not be called directly; use Types.areEqual */
	abstract equals(other: TypeInfo): boolean;

	abstract toString(): string;

}

export namespace Types {

	export function isSupersetOf(a: TypeInfo, b: TypeInfo) {
		return a.isSupersetOf(b) || b.isSubsetOf(a);
	}

	export function areEqual(a: TypeInfo, b: TypeInfo) {
		return a.equals(b) || b.equals(a);
	}

	export const Nil = new class extends TypeInfo {

		isSupersetOf(other: TypeInfo): boolean {
			return other instanceof Nil.constructor;
		}

		equals(other: TypeInfo): boolean {
			return other instanceof Nil.constructor;
		}

		toString(): string {
			return "nil";
		}

	};

	export const Int = new class extends TypeInfo {

		isSupersetOf(other: TypeInfo): boolean {
			return other instanceof Int.constructor;
		}

		equals(other: TypeInfo): boolean {
			return other instanceof Int.constructor;
		}

		toString(): string {
			return "int";
		}

	};

	export const Bool = new class extends TypeInfo {

		isSupersetOf(other: TypeInfo): boolean {
			return other instanceof Bool.constructor;
		}

		equals(other: TypeInfo): boolean {
			return other instanceof Bool.constructor;
		}

		toString(): string {
			return "bool";
		}

	};

	export const String = new class extends TypeInfo {

		isSupersetOf(other: TypeInfo): boolean {
			return other instanceof String.constructor;
		}

		equals(other: TypeInfo): boolean {
			return other instanceof String.constructor;
		}

		toString(): string {
			return "string";
		}

	};

	export class Function extends TypeInfo {

		constructor(
			public returnType: TypeInfo,
			public argsTypes: TypeInfo[],
		) {
			super();
		}

		isSupersetOf(other: TypeInfo): boolean {
			if (other instanceof Function) {
				return isSupersetOf(this.returnType, other.returnType)
					&& this.argsTypes.length >= other.argsTypes.length
					&& other.argsTypes.every((value, i) => isSupersetOf(value, this.argsTypes[i]));
			}
			else {
				return false;
			}
		}

		equals(other: TypeInfo): boolean {
			if (other instanceof Function) {
				return areEqual(this.returnType, other.returnType)
					&& this.argsTypes.length === other.argsTypes.length
					&& this.argsTypes.every((value, i) => areEqual(value, other.argsTypes[i]));
			}
			else {
				return false;
			}
		}

		toString(): string {
			return `function(${this.argsTypes.map(x => x.toString()).join(", ")}): ${this.returnType.toString()}`;
		}

	}

}

class Environment {

	variables = new Map<string, TypeInfo>();

	constructor(
		public checker: Typechecker,
		public parent?: Environment,
		public attributes: { [x: string]: boolean } = {},
	) { }

	declare(span: Span, name: string, typeInfo: TypeInfo) {
		if (this.variables.has(name)) {
			this.checker.diagnostics.push({
				kind: "error",
				message: `redeclaration of '${name}'`,
				spans: [span],
			});
		}
		else {
			this.variables.set(name, typeInfo);
		}
	}

	get(name: string): TypeInfo | undefined {
		return this.variables.get(name) ?? this.parent?.get(name);
	}

}

interface TypecheckerResult {
	publicTypes: { [x: string]: TypeInfo };
	dependsOn: string[];
	typeAnnotations: WeakMap<AST.Expr, TypeInfo>;
}

class Typechecker {

	result: TypecheckerResult = {
		publicTypes: {},
		dependsOn: [],
		typeAnnotations: new WeakMap,
	};

	constructor(public ast: AST.File, public diagnostics: Diagnostic[]) { }

	env = new Environment(this, undefined, {
		private: false,
	});

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
				this.env.declare(impor.span, symbol, Types.Nil); // TODO: proper types
			}
		}
		for (const decl of node.body) {
			if (decl.kind === "scoped-attribute") {
				this.check(decl);
			}
			else {
				this.result.publicTypes[decl.name] = Types.Nil; // TODO: proper types

				if (decl.kind === "func-decl") {
					this.env.declare(decl.span, decl.name, Types.Nil); // TODO: proper types
				}
				else if (decl.kind === "var-decl") {
					this.env.declare(decl.span, decl.name, Types.Nil); // TODO: proper types
				}
			}
		}
		for (const decl of node.body) {
			if (decl.kind === "func-decl") {
				// don't forget about checkFuncDecl
				this.newEnv();
				for (const param of decl.params) {
					this.env.declare(decl.span, param.name, Types.Nil); // TODO: proper types
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
		}
	}

	checkFuncCall(node: AST.FuncCall) {
		this.check(node.func);
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

	checkReturn(node: AST.Return) {
		if (node.value) {
			this.check(node.value);
		}
	}

	checkInlineLua(node: AST.InlineLua) {}

	checkFuncDecl(node: AST.FuncDecl) {
		// don't forget about checkFile
		this.env.declare(node.span, node.name, Types.Nil); // TODO: proper types
		this.newEnv();
		for (const param of node.params) {
			this.env.declare(node.span, param.name, Types.Nil); // TODO: proper types
		}
		for (const stat of node.body) {
			this.check(stat);
		}
		this.exitEnv();
	}

	checkVarDecl(node: AST.VarDecl) {
		// don't forget about checkFile
		this.check(node.value);
		this.env.declare(node.span, node.name, Types.Nil); // TODO: proper types
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

}

export function typecheck(ast: AST.File, diagnostics: Diagnostic[]): TypecheckerResult {
	const checker = new Typechecker(ast, diagnostics);
	checker.check(ast);
	return checker.result;
}
