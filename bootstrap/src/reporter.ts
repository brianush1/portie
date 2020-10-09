import { Diagnostic } from "./diagnostic";

export function report(diagnostics: Diagnostic[]): "errors" | "no-errors" {
	let result = false;
	for (const diag of diagnostics) {
		console.log(diag.message);
		if (diag.kind === "error") {
			result = true;
		}
	}
	return result ? "errors" : "no-errors";
}
