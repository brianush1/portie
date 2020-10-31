import { Pipe } from "stream";
import { Diagnostic, Span } from "./diagnostic";
import { Lexer, Token, TokenOf } from "./lexer";
import { report } from "./reporter";

export namespace AST {

	// Miscellaneous:

	export interface Condition {
		inverted: boolean;
		decl?: Decl;
		value: Expr;
	}

	export interface Import {
		span: Span;
		module: string;
		symbols: string[];
	}

	export interface File {
		kind: "file";
		span: Span;
		imports: Import[];
		body: Decl[];
	}

	// Expression statements:

	export type ExprStat = FuncCall | NewCall | PipeCall | InlineLua;

	export interface FuncCall {
		kind: "func-call";
		span: Span;
		func: Expr;
		args: Expr[];
	}

	export interface NewCall {
		kind: "new-call";
		span: Span;
		classObj: Expr;
		args: Expr[];
		fields: {
			name: string;
			span: Span;
			value: Expr;
		}[];
	}

	export interface PipeCall {
		kind: "pipe-call";
		span: Span;
		base: Expr;
		func: string;
		args: Expr[];
	}

	// Statements:

	export type Stat = Decl | ExprStat
		| If | While | For | Return
		| Assign;

	export interface If {
		kind: "if";
		span: Span;
		cond: Condition;
		body: Stat[];
		elseBody: Stat[];
	}

	export interface While {
		kind: "while";
		span: Span;
		cond: Condition;
		body: Stat[];
	}

	export interface For {
		kind: "for";
		span: Span;
		name: string;
		type?: Type;
		value: Expr;
		body: Stat[];
	}

	export interface Return {
		kind: "return";
		span: Span;
		value?: Expr;
	}

	export interface InlineLua {
		kind: "inline-lua";
		span: Span;
		value: string;
	}

	export interface Assign {
		kind: "assign";
		span: Span;
		lvalue: VarAccess | Index;
		rvalue: Expr;
	}

	// Declarations:

	export type Decl = Attribute | FuncDecl | VarDecl | ClassDecl;

	export type Param = {
		name: string;
		type: AST.Type;
		defaultValue?: Expr;
	} | {
		name: string;
		type: AST.Type;
		defaultValue: undefined;
		rest: true;
	};

	export interface FuncDecl {
		kind: "func-decl";
		span: Span;
		name: string;
		params: Param[];
		returnType?: AST.Type;
		body: Stat[];
	}

	export interface VarDecl {
		kind: "var-decl";
		span: Span;
		name: string;
		isConst: boolean;
		type?: Type;
		value: Expr;
	}

	export interface ClassDecl {
		kind: "class-decl";
		span: Span;
		name: string;
		base?: Expr;
		body: (Decl | {
			kind: "empty-var-decl";
			span: Span;
			name: string;
			isConst: boolean;
			type: Type;
		})[];
	}

	export type Attribute = ScopedAttribute;

	export interface ScopedAttribute {
		kind: "scoped-attribute";
		span: Span;
		which: "private";
		value?: Expr;
	}

	// Expressions:

	export type Expr = ExprStat | StrLiteral | NumLiteral
		| NilLiteral | BoolLiteral | ThisLiteral | SuperLiteral
		| VarAccess | TableLiteral | ArrayLiteral | FuncLiteral
		| BinOp | UnOp | Index | IfExpr;

	export interface StrLiteral {
		kind: "str-literal";
		span: Span;
		value: string;
	}

	export interface NumLiteral {
		kind: "num-literal";
		span: Span;
		value: number;
	}

	export interface NilLiteral {
		kind: "nil-literal";
		span: Span;
	}

	export interface BoolLiteral {
		kind: "bool-literal";
		span: Span;
		value: boolean;
	}

	export interface ThisLiteral {
		kind: "this-literal";
		span: Span;
	}

	export interface SuperLiteral {
		kind: "super-literal";
		span: Span;
	}

	export interface VarAccess {
		kind: "var-access";
		span: Span;
		name: string;
	}

	export interface TableLiteral {
		kind: "table-literal";
		span: Span;
		fields: {
			key: Expr;
			isConst: boolean;
			type?: Type;
			value: Expr;
		}[];
	}

	export interface ArrayLiteral {
		kind: "array-literal";
		span: Span;
		values: Expr[];
	}

	export interface FuncLiteral {
		kind: "func-literal";
		span: Span;
		params: Param[];
		returnType?: AST.Type;
		body: Stat[];
	}

	export type BinOperator =
		| "||"
		| "&&"
		| "<" | ">" | "<=" | ">=" | "==" | "!="
		| ".."
		| "+" | "-"
		| "*" | "/" | "%";

	export interface BinOp {
		kind: "bin-op";
		span: Span;
		op: BinOperator;
		lhs: Expr;
		rhs: Expr;
	}

	export type UnOperator = "-" | "!" | "~";

	export interface UnOp {
		kind: "un-op";
		span: Span;
		op: UnOperator;
		value: Expr;
	}

	export interface Index {
		kind: "index";
		span: Span;
		base: Expr;
		args: Expr[];
	}

	export interface IfExpr {
		kind: "if-expr";
		span: Span;
		cond: Condition;
		value: Expr;
		elseValue: Expr;
	}

	// Types:

	export type Type = NamedType | NilType | TypeStrLiteral
		| TypeNumLiteral | ComplementType | BinTypeOp | TypeIndex;

	export interface NamedType {
		kind: "named-type";
		span: Span;
		name: string;
	}

	export interface NilType {
		kind: "nil-type";
		span: Span;
	}

	export interface TypeStrLiteral {
		kind: "type-str-literal";
		span: Span;
		value: string;
	}

	export interface TypeNumLiteral {
		kind: "type-num-literal";
		span: Span;
		value: number;
	}

	export interface ComplementType {
		kind: "complement-type";
		span: Span;
		value: Type;
	}

	export type BinTypeOperator = "|" | "^" | "&";

	export interface BinTypeOp {
		kind: "bin-type-op";
		span: Span;
		op: BinTypeOperator;
		lhs: Type;
		rhs: Type;
	}

	export interface TypeIndex {
		kind: "type-index";
		span: Span;
		base: Type;
		args: Type[];
	}

}

abstract class PrefixParselet {

	abstract parse(lexer: Lexer, parser: Parser, token: Token): AST.Expr | undefined;

}

class UnaryParselet extends PrefixParselet {

	constructor(public precedence: number, public op: AST.UnOperator) {
		super();
	}

	parse(lexer: Lexer, parser: Parser, token: Token): AST.Expr | undefined {
		const value = parser.expr(this.precedence);
		return value ? {
			kind: "un-op",
			span: combineSpans(token.span, lexer.last().span),
			op: this.op,
			value,
		} : value;
	}

}

const PREFIX_PARSELETS: { [x: string]: PrefixParselet } = {
	"-": new UnaryParselet(700, "-"),
	"~": new UnaryParselet(700, "~"),
	"!": new UnaryParselet(700, "!"),
};

abstract class InfixParselet {

	constructor(public precedence: number) { }

	abstract parse(lhs: AST.Expr, lexer: Lexer, parser: Parser, token: Token): AST.Expr | undefined;

}

class BinaryParselet extends InfixParselet {

	constructor(precedence: number, public op: AST.BinOperator, public assoc: "left" | "right") {
		super(precedence);
	}

	parse(lhs: AST.Expr, lexer: Lexer, parser: Parser, token: Token): AST.Expr | undefined {
		const rhs = parser.expr(this.precedence + (this.assoc === "left" ? 1 : 0));
		return rhs ? {
			kind: "bin-op",
			span: combineSpans(lhs.span, lexer.last().span),
			op: this.op,
			lhs, rhs,
		} : rhs;
	}

}

const INFIX_PARSELETS: { [x: string]: InfixParselet } = {
	"||": new BinaryParselet(100, "||", "left"),

	"&&": new BinaryParselet(200, "&&", "left"),

	"<": new BinaryParselet(300, "<", "left"),
	">": new BinaryParselet(300, ">", "left"),
	"<=": new BinaryParselet(300, "<=", "left"),
	">=": new BinaryParselet(300, ">=", "left"),
	"==": new BinaryParselet(300, "==", "left"),
	"!=": new BinaryParselet(300, "!=", "left"),

	"..": new BinaryParselet(450, "..", "right"),

	"+": new BinaryParselet(500, "+", "left"),
	"-": new BinaryParselet(500, "-", "left"),

	"*": new BinaryParselet(600, "*", "left"),
	"/": new BinaryParselet(600, "/", "left"),
	"%": new BinaryParselet(600, "%", "left"),

	"(": new class extends InfixParselet {
		parse(lhs: AST.Expr, lexer: Lexer, parser: Parser, token: Token): AST.Expr | undefined {
			const args: AST.Expr[] = [];
			while (lexer.until(["symbol", ")"])) {
				const expr = parser.expr();
				if (expr) {
					args.push(expr);
				}
				if (!lexer.tryNext(["symbol", ","])) {
					break;
				}
			}
			lexer.next(["symbol", ")"], "expected ')' to close argument list");
			return {
				kind: "func-call",
				span: combineSpans(lhs.span, lexer.last().span),
				func: lhs,
				args,
			};
		}
	}(800),

	"[": new class extends InfixParselet {
		parse(lhs: AST.Expr, lexer: Lexer, parser: Parser, token: Token): AST.Expr | undefined {
			const args: AST.Expr[] = [];
			while (lexer.until(["symbol", "]"])) {
				const expr = parser.expr();
				if (expr) {
					args.push(expr);
				}
				if (!lexer.tryNext(["symbol", ","])) {
					break;
				}
			}
			lexer.next(["symbol", "]"], "expected ']' to close index");
			return {
				kind: "index",
				span: combineSpans(lhs.span, lexer.last().span),
				base: lhs,
				args,
			};
		}
	}(800),

	".": new class extends InfixParselet {
		parse(lhs: AST.Expr, lexer: Lexer, parser: Parser, token: Token): AST.Expr | undefined {
			const keyToken = lexer.next("name", "expected name in index");
			if (!keyToken) {
				return undefined;
			}
			const key = keyToken.value;
			return {
				kind: "index",
				span: combineSpans(lhs.span, lexer.last().span),
				base: lhs,
				args: [
					{
						kind: "str-literal",
						span: keyToken.span,
						value: key,
					},
				],
			};
		}
	}(800),

	":": new class extends InfixParselet {
		parse(lhs: AST.Expr, lexer: Lexer, parser: Parser, token: Token): AST.Expr | undefined {
			const func = lexer.next("name", "expected name in pipe")?.value;
			if (!func) {
				return undefined;
			}
			lexer.next(["symbol", "("], "expected '(' to open argument list");
			const args: AST.Expr[] = [];
			while (lexer.until(["symbol", ")"])) {
				const expr = parser.expr();
				if (expr) {
					args.push(expr);
				}
				if (!lexer.tryNext(["symbol", ","])) {
					break;
				}
			}
			lexer.next(["symbol", ")"], "expected ')' to close argument list");
			return {
				kind: "pipe-call",
				span: combineSpans(lhs.span, lexer.last().span),
				base: lhs,
				func,
				args,
			};
		}
	}(800),
};

abstract class InfixTypeParselet {

	constructor(public precedence: number) { }

	abstract parse(lhs: AST.Type, lexer: Lexer, parser: Parser, token: Token): AST.Type | undefined;

}

class BinaryTypeParselet extends InfixTypeParselet {

	constructor(precedence: number, public op: AST.BinTypeOperator, public assoc: "left" | "right") {
		super(precedence);
	}

	parse(lhs: AST.Type, lexer: Lexer, parser: Parser, token: Token): AST.Type | undefined {
		const rhs = parser.type(this.precedence + (this.assoc === "left" ? 1 : 0));
		return rhs ? {
			kind: "bin-type-op",
			span: combineSpans(lhs.span, lexer.last().span),
			op: this.op,
			lhs, rhs,
		} : rhs;
	}

}

const INFIX_TYPE_PARSELETS: { [x: string]: InfixTypeParselet } = {
	"|": new BinaryTypeParselet(100, "|", "left"),

	"^": new BinaryTypeParselet(150, "^", "left"),

	"&": new BinaryTypeParselet(200, "&", "left"),

	"[": new class extends InfixTypeParselet {
		parse(lhs: AST.Type, lexer: Lexer, parser: Parser, token: Token): AST.Type | undefined {
			const args: AST.Type[] = [];
			while (lexer.until(["symbol", "]"])) {
				const type = parser.type();
				if (type) {
					args.push(type);
				}
				if (!lexer.tryNext(["symbol", ","])) {
					break;
				}
			}
			lexer.next(["symbol", "]"], "expected ']' to close index");
			return {
				kind: "type-index",
				span: combineSpans(lhs.span, lexer.last().span),
				base: lhs,
				args,
			};
		}
	}(800),

	".": new class extends InfixTypeParselet {
		parse(lhs: AST.Type, lexer: Lexer, parser: Parser, token: Token): AST.Type | undefined {
			const keyToken = lexer.next("name", "expected name in index");
			if (!keyToken) {
				return undefined;
			}
			const key = keyToken.value;
			return {
				kind: "type-index",
				span: combineSpans(lhs.span, lexer.last().span),
				base: lhs,
				args: [
					{
						kind: "type-str-literal",
						span: keyToken.span,
						value: key,
					},
				],
			};
		}
	}(800),
};

function combineSpans(span: Span, ...spans: Span[]): Span {
	spans = [span, ...spans];
	return {
		file: span.file,
		start: spans.reduce((a, b) => Math.min(a, b.start), Infinity),
		end: spans.reduce((a, b) => Math.max(a, b.end), -Infinity),
	};
}

export class Parser {

	private file: number;
	private diagnostics: Diagnostic[];
	private lexer: Lexer;

	constructor(file: number, source: string, diagnostics: Diagnostic[]) {
		this.file = file;
		this.lexer = new Lexer(file, source, diagnostics);
		this.diagnostics = diagnostics;
	}

	private semi() {
		this.lexer.next(["symbol", ";"], "expected semicolon");
	}

	private body(kind: string): AST.Stat[] {
		if (this.lexer.tryNext(["symbol", "{"])) {
			const stats: AST.Stat[] = [];
			while (this.lexer.until(["symbol", "}"])) {
				const stat = this.stat();
				if (stat)
					stats.push(stat);
			}
			this.lexer.next(["symbol", "}"], `expected '}' to close ${kind} body`);
			return stats;
		}
		else {
			const stat = this.stat();
			if (stat)
				return [stat];
			return [];
		}
	}

	private condition(): AST.Condition {
		const inverted = this.lexer.tryNext(["symbol", "!"]) !== undefined;
		this.lexer.next(["symbol", "("], "to open condition");
		const state = this.lexer.save();
		let decl: AST.Decl | undefined;
		let value = this.exprOrNil();
		if (!this.lexer.isNext(["symbol", ")"])) {
			this.lexer.restore(state);
			decl = this.decl();
			value = this.exprOrNil();
		}
		this.lexer.next(["symbol", ")"], "to close condition");
		return {
			inverted,
			decl,
			value,
		};
	}

	parse(): AST.File {
		const imports: AST.Import[] = [];
		while (this.lexer.tryNext(["keyword", "import"])) {
			const startToken = this.lexer.last();
			let module = this.lexer.next("name", "expected name in import")?.value;
			while (module && this.lexer.tryNext(["symbol", "."])) {
				const name = this.lexer.next("name", "expected name in import")?.value;
				if (name) {
					module += "." + name;
				}
				else {
					break;
				}
			}
			this.lexer.next(["symbol", ":"], "expected ':' to separate symbols from module");
			const symbols = [];
			while (this.lexer.until(["symbol", ";"])) {
				const symbol = this.lexer.next("name", "expected name in import")?.value;
				if (symbol) {
					symbols.push(symbol);
				}
				if (!this.lexer.tryNext(["symbol", ","])) {
					break;
				}
			}
			this.semi();
			if (module)
				imports.push({ span: combineSpans(startToken.span, this.lexer.last().span),
					module, symbols });
		}
		const body: AST.Decl[] = [];
		while (!this.lexer.eof()) {
			const decl = this.decl();
			if (decl !== undefined) {
				body.push(decl);
			}
		}
		return {
			kind: "file",
			span: this.lexer.fileSpan(),
			imports, body,
		};
	}

	// Statements:

	stat(): AST.Stat | undefined {
		const start = this.lexer.peek();
		if (this.lexer.tryNext(["keyword", "if"])) {
			const cond = this.condition();
			const body = this.body("if");
			let elseBody: AST.Stat[] = [];
			if (this.lexer.tryNext(["keyword", "else"])) {
				elseBody = this.body("else");
			}
			return {
				kind: "if",
				span: combineSpans(start.span, this.lexer.last().span),
				cond, body, elseBody,
			};
		}
		else if (this.lexer.tryNext(["keyword", "while"])) {
			const cond = this.condition();
			const body = this.body("while");
			return {
				kind: "while",
				span: combineSpans(start.span, this.lexer.last().span),
				cond, body,
			};
		}
		else if (this.lexer.tryNext(["keyword", "for"])) {
			this.lexer.next(["symbol", "("], "expected '(' to open for statement");
			const name = this.lexer.next("name", "expected name in for statement")?.value;
			if (!name) {
				return undefined;
			}
			this.lexer.next(["symbol", ":"], "expected ':' in for statement");
			const type = this.lexer.isNext(["symbol", "="]) ? undefined : this.type();
			this.lexer.next(["symbol", "="], "expected '=' in for statement");
			this.lexer.next(["symbol", "..."], "expected '...' in for statement");
			const value = this.exprOrNil();
			this.lexer.next(["symbol", ")"], "expected ')' to close for statement");
			const body = this.body("for");
			return {
				kind: "for",
				span: combineSpans(start.span, this.lexer.last().span),
				name, type, value, body,
			};
		}
		else if (this.lexer.tryNext(["keyword", "return"])) {
			let value: AST.Expr | undefined;
			if (!this.lexer.isNext(["symbol", ";"])) {
				value = this.expr();
			}
			this.semi();
			return {
				kind: "return",
				span: combineSpans(start.span, this.lexer.last().span),
				value,
			};
		}
		else {
			const state = this.lexer.save();
			const stat = this.expr();
			if (this.lexer.tryNext(["symbol", ";"])) {
				if (stat?.kind === "func-call" || stat?.kind === "pipe-call"
					|| stat?.kind === "new-call" || stat?.kind === "inline-lua") {
					return stat;
				}
				else if (stat === undefined) {
					this.lexer.restore(state);
					this.diagnostics.push({
						kind: "error",
						message: "expected statement",
						spans: [this.lexer.next().span],
					});
					return undefined;
				}
				else {
					this.diagnostics.push({
						kind: "error",
						message: "expected statement, got expression",
						spans: [stat.span],
					});
					return undefined;
				}
			}
			else if ((stat?.kind === "var-access" || stat?.kind === "index")
				&& this.lexer.tryNext(["symbol", "="])) {
				const rvalue = this.exprOrNil();
				this.semi();
				return {
					kind: "assign",
					span: combineSpans(stat.span, this.lexer.last().span),
					lvalue: stat,
					rvalue,
				};
			}
			else {
				this.lexer.restore(state);
				return this.decl();
			}
		}
	}

	// Declarations:

	func(): {
		span: Span;
		body: AST.Stat[];
		params: AST.Param[];
		returnType?: AST.Type;
	} | undefined {
		const start = this.lexer.peek();
		this.lexer.next(["symbol", "("], "expected '(' to open parameter list");
		const params: AST.Param[] = [];
		while (this.lexer.until(["symbol", ")"])) {
			let rest = this.lexer.tryNext(["symbol", "..."]);
			const name = this.lexer.next("name", "expected parameter name")?.value;
			this.lexer.next(["symbol", ":"], "expected ':' to separate parameter name and type");
			const type = this.type();
			let defaultValue: AST.Expr | undefined;
			if (this.lexer.tryNext(["symbol", "="]) && !rest) {
				defaultValue = this.exprOrNil();
			}
			if (name && type) {
				if (rest) {
					params.push({ name, type, rest: true });
				}
				else {
					params.push({ name, type, defaultValue });
				}
			}
			if (!this.lexer.tryNext(["symbol", ","])) {
				break;
			}
		}
		this.lexer.next(["symbol", ")"], "expected ')' to close parameter list");
		let returnType: AST.Type | undefined;
		if (this.lexer.tryNext(["symbol", ":"])) {
			returnType = this.type();
		}
		let body: AST.Stat[];
		let arrowToken;
		if (arrowToken = this.lexer.tryNext(["symbol", "=>"])) {
			body = [
				{
					kind: "return",
					span: arrowToken.span,
					value: this.exprOrNil(),
				},
			];
		}
		else {
			body = this.body("function");
		}
		return {
			span: combineSpans(start.span, this.lexer.last().span),
			body, params, returnType,
		};
	}

	decl(): AST.Decl | undefined {
		const start = this.lexer.peek();
		if (this.lexer.tryNext(["keyword", "private"])) {
			this.lexer.next(["symbol", ":"], "expected ':' after scoped attribute 'private'");
			return {
				kind: "scoped-attribute",
				span: combineSpans(start.span, this.lexer.last().span),
				which: "private",
			};
		}

		if (this.lexer.tryNext(["keyword", "class"])) {
			const name = this.lexer.next("name", "expected name")?.value;
			if (name === undefined) {
				return undefined;
			}
			let base: AST.Expr | undefined;
			if (this.lexer.tryNext(["keyword", "extends"])) {
				base = this.expr();
			}
			const body: AST.ClassDecl["body"] = [];
			this.lexer.next(["symbol", "{"], "expected '{' to open class body");
			while (this.lexer.until(["symbol", "}"])) {
				const state = this.lexer.save();
				const isConst = this.lexer.tryNext(["keyword", "const"]) ? true : false;
				const name = this.lexer.tryNext("name");
				if (name && this.lexer.tryNext(["symbol", ":"])
					&& !this.lexer.tryNext(["symbol", "="])) {
					const type = this.type();
					if (!this.lexer.tryNext(["symbol", ";"])) {
						this.lexer.restore(state);
					}
					else {
						if (type !== undefined) {
							body.push({
								kind: "empty-var-decl",
								span: combineSpans(name.span, this.lexer.last().span),
								isConst,
								name: name.value,
								type,
							});
						}
						continue;
					}
				}
				else {
					this.lexer.restore(state);
				}
				const decl = this.decl();
				if (!decl) {
					continue;
				}
				body.push(decl);
			}
			this.lexer.next(["symbol", "}"], "expected '}' to close class body");
			return {
				kind: "class-decl",
				span: combineSpans(start.span, this.lexer.last().span),
				name, base, body,
			};
		}

		const constToken = this.lexer.tryNext(["keyword", "const"]);
		const isConst = constToken ? true : false;
		const nameToken = this.lexer.tryNext(["keyword", "this"])
			?? this.lexer.next("name", "expected name in declaration");
		if (nameToken === undefined)
			return undefined;
		const name = nameToken.value;
		if (this.lexer.isNext(["symbol", "("])) {
			if (constToken) {
				this.diagnostics.push({
					kind: "error",
					message: "function declarations are implicitly 'const'",
					spans: [constToken.span],
				});
			}
			const func = this.func();
			if (func === undefined) {
				return undefined;
			}
			return {
				kind: "func-decl",
				span: combineSpans(nameToken.span, func.span),
				body: func.body,
				params: func.params,
				returnType: func.returnType,
				name,
			};
		}
		else {
			let type: AST.Type | undefined;
			this.lexer.next(["symbol", ":"], "expected ':' to start declaration");
			if (!this.lexer.isNext(["symbol", "="])) {
				type = this.type();
			}
			this.lexer.next(["symbol", "="], "expected '=' to separate variable from initial value");
			const value = this.exprOrNil();
			this.semi();
			return {
				kind: "var-decl",
				span: combineSpans(nameToken.span, this.lexer.last().span),
				isConst, name, type, value,
			};
		}
	}

	// Expressions:

	atom(): AST.Expr | undefined {
		const start = this.lexer.peek();
		const startState = this.lexer.save();
		let token;
		if (token = this.lexer.tryNext("string")) {
			return {
				kind: "str-literal",
				span: token.span,
				value: token.value,
			};
		}
		else if (token = this.lexer.tryNext("integer")) {
			return {
				kind: "num-literal",
				span: token.span,
				value: Number(token.value),
			};
		}
		else if (this.lexer.tryNext(["keyword", "if"])) {
			const cond = this.condition();
			const value = this.exprOrNil();
			this.lexer.next(["keyword", "else"], "expected 'else' in 'if' expression");
			const elseValue = this.exprOrNil();
			return {
				kind: "if-expr",
				span: combineSpans(start.span, this.lexer.last().span),
				cond, value, elseValue,
			};
		}
		else if (token = this.lexer.tryNext(["keyword", "nil"])) {
			return {
				kind: "nil-literal",
				span: combineSpans(token.span),
			};
		}
		else if (token = this.lexer.tryNext(["keyword", "true"])) {
			return {
				kind: "bool-literal",
				span: combineSpans(token.span),
				value: true,
			};
		}
		else if (token = this.lexer.tryNext(["keyword", "false"])) {
			return {
				kind: "bool-literal",
				span: combineSpans(token.span),
				value: false,
			};
		}
		else if (token = this.lexer.tryNext(["keyword", "this"])) {
			return {
				kind: "this-literal",
				span: combineSpans(token.span),
			};
		}
		else if (token = this.lexer.tryNext(["keyword", "super"])) {
			return {
				kind: "super-literal",
				span: combineSpans(token.span),
			};
		}
		else if (token = this.lexer.tryNext(["keyword", "new"])) {
			const classObj = this.expr(900);
			if (!classObj) {
				return undefined;
			}
			const args: AST.Expr[] = [];
			if (this.lexer.tryNext(["symbol", "("])) {
				while (this.lexer.until(["symbol", ")"])) {
					const expr = this.expr();
					if (expr !== undefined) {
						args.push(expr);
					}
					if (!this.lexer.tryNext(["symbol", ","])) {
						break;
					}
				}
				this.lexer.next(["symbol", ")"], "expected ')' to close instantiation arguments");
			}
			const fields: AST.NewCall["fields"] = [];
			if (this.lexer.tryNext(["symbol", "{"])) {
				while (this.lexer.until(["symbol", "}"])) {
					const startSpan = this.lexer.peek().span;
					const name = this.lexer.next("name", "expected name")?.value;
					this.lexer.next(["symbol", "="], "expected '=' to separate lvalue from rvalue");
					const value = this.expr();
					if (value !== undefined && name !== undefined) {
						fields.push({
							name, value,
							span: combineSpans(startSpan, this.lexer.last().span),
						});
					}
					if (!this.lexer.tryNext(["symbol", ","])
						&& !this.lexer.tryNext(["symbol", ";"])) {
						break;
					}
				}
				this.lexer.next(["symbol", "}"], "expected '}' to close instantiation arguments");
			}
			return {
				kind: "new-call",
				span: combineSpans(token.span),
				classObj, args, fields,
			};
		}
		else if (this.lexer.tryNext(["keyword", "inline_lua"])) {
			const value = this.lexer.next("string", "expected string")?.value;
			if (!value) {
				return undefined;
			}
			return {
				kind: "inline-lua",
				span: combineSpans(start.span, this.lexer.last().span),
				value,
			};
		}
		else if (token = this.lexer.tryNext("name")) {
			return {
				kind: "var-access",
				span: token.span,
				name: token.value,
			};
		}
		else if (token = this.lexer.tryNext(["symbol", "{"])) {
			const fields: AST.TableLiteral["fields"] = [];
			while (this.lexer.until(["symbol", "}"])) {
				// const constToken = this.lexer.tryNext(["keyword", "const"]);
				const isConst = /* constToken ? true : */ false;
				let key: AST.Expr;
				if (this.lexer.tryNext(["symbol", "["])) {
					key = this.exprOrNil();
					this.lexer.next(["symbol", "]"], "expected ']' to close key");
				}
				else {
					const nameToken = this.lexer.next("name", "expected key in field declaration");
					if (nameToken === undefined)
						return undefined;
					key = {
						kind: "str-literal",
						span: nameToken.span,
						value: nameToken.value,
					};
				}
				this.lexer.next(["symbol", ":"], "expected ':' to begin field declaration");
				let type: AST.Type | undefined;
				if (!this.lexer.isNext(["symbol", "="])) {
					type = this.type();
				}
				this.lexer.next(["symbol", "="], "expected '=' to separate field from initial value");
				const value = this.exprOrNil();
				fields.push({ key, isConst, type, value });
				if (this.lexer.isNext(["symbol", ";"]) || this.lexer.isNext(["symbol", ","])) {
					this.lexer.next();
				}
				else {
					break;
				}
			}
			this.lexer.next(["symbol", "}"], "expected '}' to close table literal");
			return {
				kind: "table-literal",
				span: token.span,
				fields,
			};
		}
		else if (token = this.lexer.tryNext(["symbol", "["])) {
			const values: AST.Expr[] = [];
			while (this.lexer.until(["symbol", "]"])) {
				const value = this.exprOrNil();
				values.push(value);
				if (this.lexer.isNext(["symbol", ","])) {
					this.lexer.next();
				}
				else {
					break;
				}
			}
			this.lexer.next(["symbol", "]"], "expected ']' to close array literal");
			return {
				kind: "array-literal",
				span: token.span,
				values,
			};
		}
		else if (token = this.lexer.tryNext(["symbol", "("])) {
			let count = 1;
			while (count > 0 && !this.lexer.eof()) {
				if (this.lexer.tryNext(["symbol", "("])) {
					count++;
				}
				else if (this.lexer.tryNext(["symbol", ")"])) {
					count--;
				}
				else {
					this.lexer.next();
				}
			}
			let isFunction = true;
			if (this.lexer.tryNext(["symbol", ":"])) {
				const type = this.type();
				if (!type) {
					isFunction = false;
				}
			}
			isFunction &&= this.lexer.isNext(["symbol", "{"]) || this.lexer.isNext(["symbol", "=>"]);
			this.lexer.restore(startState);
			if (!isFunction) {
				this.lexer.next();
				const value = this.exprOrNil();
				this.lexer.next(["symbol", ")"], "expected ')' to close parentheses");
				return value;
			}
			else {
				const func = this.func();
				if (func === undefined) {
					return undefined;
				}
				return {
					kind: "func-literal",
					span: combineSpans(token.span, func.span),
					body: func.body,
					params: func.params,
					returnType: func.returnType,
				};
			}
		}
		else {
			this.diagnostics.push({
				kind: "error",
				message: "expected expression",
				spans: [this.lexer.next().span],
			});
			return undefined;
		}
	}

	peekInfixParselet(): InfixParselet | undefined {
		if (this.lexer.isNext("symbol")) {
			const value = (this.lexer.peek() as TokenOf<"symbol">).value;
			return INFIX_PARSELETS[value];
		}
		else {
			return undefined;
		}
	}

	peekPrefixParselet(): PrefixParselet | undefined {
		if (this.lexer.isNext("symbol")) {
			const value = (this.lexer.peek() as TokenOf<"symbol">).value;
			return PREFIX_PARSELETS[value];
		}
		else {
			return undefined;
		}
	}

	expr(precedence: number = 0): AST.Expr | undefined {
		let lhs: AST.Expr | undefined;
		const parselet = this.peekPrefixParselet();
		if (parselet === undefined) {
			lhs = this.atom();
		}
		else {
			lhs = parselet.parse(this.lexer, this, this.lexer.next());
		}
		if (!lhs) {
			return undefined;
		}

		while (true) {
			const parselet = this.peekInfixParselet();
			if (parselet === undefined || parselet.precedence < precedence) {
				break;
			}
			const expr = parselet.parse(lhs, this.lexer, this, this.lexer.next());
			if (expr !== undefined) {
				lhs = expr;
			}
			else {
				break;
			}
		}

		return lhs;
	}

	exprOrNil(): AST.Expr {
		const firstToken = this.lexer.peek();
		const result = this.expr();
		if (result) {
			return result;
		}
		else {
			return {
				kind: "nil-literal",
				span: firstToken.span,
			};
		}
	}

	// Types:

	typeAtom(): AST.Type | undefined {
		let token;
		if (token = this.lexer.tryNext("name")) {
			return {
				kind: "named-type",
				span: token.span,
				name: token.value,
			};
		}
		else if (token = this.lexer.tryNext("string")) {
			return {
				kind: "type-str-literal",
				span: token.span,
				value: token.value,
			};
		}
		else if (token = this.lexer.tryNext("integer")) {
			return {
				kind: "type-num-literal",
				span: token.span,
				value: Number(token.value),
			};
		}
		else if (token = this.lexer.tryNext(["keyword", "nil"])) {
			return {
				kind: "nil-type",
				span: token.span,
			};
		}
		else if (token = this.lexer.tryNext(["symbol", "~"])) {
			const value = this.type(700);
			if (!value) {
				return undefined;
			}
			return {
				kind: "complement-type",
				span: combineSpans(token.span, value.span),
				value: value,
			};
		}
		else {
			this.diagnostics.push({
				kind: "error",
				message: "expected type",
				spans: [this.lexer.next().span],
			});
			return undefined;
		}
	}

	peekInfixTypeParselet(): InfixTypeParselet | undefined {
		if (this.lexer.isNext("symbol")) {
			const value = (this.lexer.peek() as TokenOf<"symbol">).value;
			return INFIX_TYPE_PARSELETS[value];
		}
		else {
			return undefined;
		}
	}

	type(precedence: number = 0): AST.Type | undefined {
		let lhs = this.typeAtom();
		if (!lhs) {
			return undefined;
		}

		while (true) {
			const parselet = this.peekInfixTypeParselet();
			if (parselet === undefined || parselet.precedence < precedence) {
				break;
			}
			const type = parselet.parse(lhs, this.lexer, this, this.lexer.next());
			if (type !== undefined) {
				lhs = type;
			}
			else {
				break;
			}
		}

		return lhs;
	}

}