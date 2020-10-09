export type OpenBracket = "(" | "[" | "{";
export type ClosedBracket = ")" | "]" | "}";
export type Bracket = OpenBracket | ClosedBracket;
export function convertBracket(bracket: OpenBracket): ClosedBracket;
export function convertBracket(bracket: ClosedBracket): OpenBracket;
export function convertBracket(bracket: OpenBracket | ClosedBracket): OpenBracket | ClosedBracket {
	switch (bracket) {
		case "(": return ")";
		case "[": return "]";
		case "{": return "}";
		case ")": return "(";
		case "]": return "[";
		case "}": return "{";
		default: throw 0;
	}
}
export function isOpenBracket(bracket: string): bracket is OpenBracket {
	return bracket === "(" || bracket === "[" || bracket === "{";
}
export function isClosedBracket(bracket: string): bracket is ClosedBracket {
	return bracket === ")" || bracket === "]" || bracket === "}";
}
export function isBracket(bracket: string): bracket is Bracket {
	return isOpenBracket(bracket) || isClosedBracket(bracket);
}
