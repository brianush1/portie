export interface Span {
	file: number;
	start: number;
	end: number;
}

export type DiagnosticKind = "error" | "warning" | "info";

export interface Diagnostic {
	kind: DiagnosticKind;
	message: string;
	spans: Span[];
}
