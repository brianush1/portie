import { convertBracket, isClosedBracket, isOpenBracket, OpenBracket } from "./bracket-utils";
import { Diagnostic, Span } from "./diagnostic";

export type TokenKind = Token["kind"];

export type Token = {
	kind: "eof";
	span: Span;
} | {
	kind: "name" | "keyword" | "symbol";
	value: string;
	span: Span;
} | {
	kind: "string";
	value: string;
	rawValue: string;
	span: Span;
} | {
	kind: "integer";
	value: bigint;
	span: Span;
} | {
	kind: "decimal";
	value: bigint;
	pow10: bigint;
	span: Span;
};

const SYMBOLS = [
	// Arith operators:
	"+", "-", "*", "/", "**",

	// Relational operators:
	"<", ">", "<=", ">=", "==", "!=",

	// Bitwise operators:
	"~", "&", "|", "^", "<<", ">>", ">>>",

	// Boolean operators:
	"!", "&&", "||",

	// Brackets:
	"(", ")", "[", "]", "{", "}",

	// Miscellaneous:
	".", ";", ":", "=>", "->",
];

SYMBOLS.sort((a, b) => b.length - a.length);

const KEYWORDS = [
	"private",
	"if", "else",
	"return",
	"typedef",
	"const",
];

function indexBefore(str: string, i: number) {
	i -= 1;
	if ((str.charCodeAt(i) >>> 10) === 0b110111) i -= 1;
	return i;
}

function codepointAt(str: string, i: number) {
	const c = str.codePointAt(i);
	if (c === undefined) return c;
	return String.fromCodePoint(c);
}

function isAlphanumeric(s: string) {
	return s.match(/^[a-zA-Z0-9_]$/) !== null;
}

function isNumeric(s: string) {
	return s.match(/^[0-9]$/) !== null;
}

function isWhitespace(s: string) {
	return s.length === 1 && " \t\r\n".includes(s);
}

export type TokenDescription = ["name" | "keyword" | "symbol", string] | TokenKind;
export type TokenOf<T> = Token & { kind: T; };

export function tokenMatches(token: Token, desc: TokenDescription) {
	if (typeof desc === "string") {
		return token.kind === desc;
	}
	else {
		return token.kind === desc[0] && token.value === desc[1];
	}
}

export interface LexerState {
	diagnostics: Diagnostic[];
	index: number;
	peekedToken: Token | undefined;
	lastToken: Token;
}

export class Lexer {

	private file: number;
	private source: string;
	private diagnostics: Diagnostic[];

	private index: number = 0;

	private peekedToken: Token | undefined;
	private lastToken!: Token;

	constructor(file: number, source: string, diagnostics: Diagnostic[]) {
		this.file = file;
		this.source = source;
		this.diagnostics = diagnostics;
	}

	private peekChar(n?: number): string | undefined {
		if (n === undefined) n = 1;
		let chars: string[] = [];
		let j = this.index;
		for (let i = 0; i < n; ++i) {
			const code = this.source.codePointAt(j);
			if (code === undefined) {
				return undefined;
			}
			const char = String.fromCodePoint(code);
			j += char.length;
			chars.push(char);
		}
		return chars.join("");
	}

	private nextChar(n?: number): string | undefined {
		const char = this.peekChar(n);
		if (char !== undefined) {
			this.index += char.length;
		}
		return char;
	}

	private eofChar(): boolean {
		return this.peekChar() === undefined;
	}

	private readWhile(predicate: (c: string) => boolean) {
		const start = this.index;
		while (true) {
			const c = this.peekChar();
			if (c === undefined || !predicate(c)) {
				break;
			}
			this.nextChar();
		}
		return this.source.substring(start, this.index);
	}

	private skipWhitespace() {
		this.readWhile(isWhitespace);
	}

	private span(start: number, end?: number): Span {
		if (end === undefined) end = this.index;
		while (true) {
			const c = codepointAt(this.source, start);
			if (c !== undefined && isWhitespace(c)) {
				start += c.length;
			}
			else {
				break;
			}
		}
		while (true) {
			const newEnd = indexBefore(this.source, end);
			const c = codepointAt(this.source, newEnd);
			// if (catch<nil> isWhitespace(try c))
			if (c !== undefined && isWhitespace(c)) {
				end = newEnd;
			}
			else {
				break;
			}
		}
		return {
			file: this.file,
			start, end,
		};
	}

	private nextImpl(): Token {
		const start = this.index;

		this.skipWhitespace();

		const c = this.peekChar();

		if (c === undefined) {
			return {
				kind: "eof",
				span: this.span(start),
			};
		}
		else if (this.peekChar(2) === "//") {
			let currentBracket: OpenBracket | undefined;
			let count: number = 0;
			const value = this.readWhile(c => {
				if (isOpenBracket(c) && currentBracket === undefined) {
					currentBracket = c;
					count = 1;
				}
				else if (isOpenBracket(c) && currentBracket === c) {
					count += 1;
				}
				else if (isClosedBracket(c) && currentBracket !== undefined
					&& convertBracket(currentBracket) === c) {
					count -= 1;
					if (count === 0) {
						currentBracket = undefined;
					}
				}

				if (c === "\n") {
					return currentBracket !== undefined;
				}
				else {
					return true;
				}
			});

			return this.nextImpl();
		}
		else if (isNumeric(c)) {
			const value = this.readWhile(isNumeric);

			return {
				kind: "integer",
				value: BigInt(value),
				span: this.span(start),
			};
		}
		else if (isAlphanumeric(c)) {
			const value = this.readWhile(isAlphanumeric);

			return {
				kind: KEYWORDS.includes(value) ? "keyword" : "name",
				value,
				span: this.span(start),
			};
		}
		else if (c === "\"") {
			this.nextChar();

			let escaped = false;

			let value = "";

			const rawValue = this.readWhile(c => {
				if (escaped) {
					switch (c) {
						case "n": value += "\n"; break;
						// TODO: the rest
						default: value += c; break;
					}
					escaped = false;
				}
				else if (c === "\\") {
					escaped = true;
				}
				else if (c === "\"") {
					return false;
				}
				else {
					value += c;
				}

				return true;
			});

			if (this.peekChar() === "\"") {
				this.nextChar();
			}
			else {
				this.diagnostics.push({
					kind: "error",
					message: "unclosed string",
					spans: [this.span(start)],
				});
			}

			return {
				kind: "string",
				value, rawValue,
				span: this.span(start),
			};
		}
		else {
			for (const sym of SYMBOLS) {
				const len = [...sym].length;
				if (this.peekChar(len) === sym) {
					this.nextChar(len);
					return {
						kind: "symbol",
						value: sym,
						span: this.span(start),
					};
				}
			}

			return {
				kind: "symbol",
				value: this.nextChar()!,
				span: this.span(start),
			};
		}
	}

	last() {
		return this.lastToken;
	}

	next(): Token;
	next<T extends TokenDescription>(desc: T, message: string): TokenOf<T> | undefined;

	next(desc?: TokenDescription, message?: string) {
		if (desc) {
			const token = this.next();
			if (tokenMatches(token, desc)) {
				return token;
			}
			else {
				this.diagnostics.push({
					kind: "error",
					message: message!.replace("$token", token.kind),
					spans: [token.span],
				});
				return undefined;
			}
		}
		else if (this.peekedToken) {
			const result = this.peekedToken;
			this.lastToken = result;
			this.peekedToken = undefined;
			return result;
		}
		else {
			return this.lastToken = this.nextImpl();
		}
	}

	peek() {
		if (this.peekedToken) {
			return this.peekedToken;
		}
		else {
			const result = this.nextImpl();
			this.peekedToken = result;
			return result;
		}
	}

	eof() {
		return this.peek().kind === "eof";
	}

	isNext(desc: TokenDescription) {
		return tokenMatches(this.peek(), desc);
	}

	until(desc: TokenDescription) {
		return !(this.isNext(desc) || this.eof());
	}

	tryNext<T extends TokenDescription>(desc: T): TokenOf<T> | undefined {
		if (this.isNext(desc)) {
			return this.next() as TokenOf<T>;
		}
		else {
			return undefined;
		}
	}

	save(): LexerState {
		return {
			diagnostics: this.diagnostics.slice(),
			index: this.index,
			peekedToken: this.peekedToken,
			lastToken: this.lastToken,
		};
	}

	restore(state: LexerState) {
		this.diagnostics.splice(0, this.diagnostics.length, ...state.diagnostics);
		this.index = state.index;
		this.peekedToken = state.peekedToken;
		this.lastToken = state.lastToken;
	}

	fileSpan(): Span {
		return {
			file: this.file,
			start: 0,
			end: this.source.length,
		};
	}

}