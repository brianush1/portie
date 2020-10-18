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

	export type ExprStat = FuncCall;

	export interface FuncCall {
		kind: "func-call";
		span: Span;
		func: Expr;
		args: Expr[];
	}

	// Statements:

	export type Stat = Decl | ExprStat
		| If | While | Return | InlineLua;

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

	// Declarations:

	export type Decl = Attribute | FuncDecl | VarDecl;

	export interface FuncDecl {
		kind: "func-decl";
		span: Span;
		name: string;
		params: {
			name: string;
			type: Type;
		}[];
		returnType: AST.Type;
		body: Stat[];
	}

	export interface VarDecl {
		kind: "var-decl";
		span: Span;
		name: string;
		isConst: boolean;
		type: Type;
		value: Expr;
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
		| NilLiteral | VarAccess | TableLiteral
		| BinOp | UnOp | Index;

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

	// Types:

	export type Type = NamedType;

	export interface NamedType {
		kind: "named-type";
		span: Span;
		name: string;
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

	"(": new class extends PrefixParselet {

		parse(lexer: Lexer, parser: Parser, token: Token): AST.Expr | undefined {
			const expr = parser.expr();
			lexer.next(["symbol", ")"], "expected ')' to close parentheses");
			return expr;
		}

	},
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
		else if (this.lexer.tryNext(["keyword", "inline_lua"])) {
			const value = this.lexer.next("string", "expected string")?.value;
			this.semi();
			if (!value) {
				return undefined;
			}
			return {
				kind: "inline-lua",
				span: combineSpans(start.span, this.lexer.last().span),
				value,
			}
		}
		else {
			const state = this.lexer.save();
			const stat = this.expr();
			if (this.lexer.tryNext(["symbol", ";"])) {
				if (stat?.kind === "func-call") {
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
		params: {
			name: string;
			type: AST.Type;
		}[];
		returnType: AST.Type;
	} | undefined {
		const start = this.lexer.peek();
		this.lexer.next(["symbol", "("], "expected '(' to open parameter list");
		const params: {
			name: string;
			type: AST.Type;
		}[] = [];
		while (this.lexer.until(["symbol", ")"])) {
			const name = this.lexer.next("name", "expected parameter name")?.value;
			this.lexer.next(["symbol", ":"], "expected ':' to separate parameter name and type");
			const type = this.type();
			if (name && type) {
				params.push({ name, type });
			}
			if (!this.lexer.tryNext(["symbol", ","])) {
				break;
			}
		}
		this.lexer.next(["symbol", ")"], "expected ')' to close parameter list");
		this.lexer.next(["symbol", ":"], "expected ':' to separate return type");
		const returnType = this.type();
		if (!returnType) {
			return undefined;
		}
		const body = this.body("function");
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

		const constToken = this.lexer.tryNext(["keyword", "const"]);
		const isConst = constToken ? true : false;
		const nameToken = this.lexer.next("name", "expected name in declaration");
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
			this.lexer.next(["symbol", ":"], "expected ':' to begin variable declaration");
			const type = this.type();
			if (type === undefined) {
				return undefined;
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
				const constToken = this.lexer.tryNext(["keyword", "const"]);
				const isConst = constToken ? true : false;
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

	type(): AST.Type | undefined {
		let token;
		if (token = this.lexer.tryNext("name")) {
			return {
				kind: "named-type",
				span: token.span,
				name: token.value,
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

}