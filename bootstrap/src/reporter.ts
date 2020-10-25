import { start } from "repl";
import { files } from ".";
import { Diagnostic } from "./diagnostic";

function getPlace(src: string, index: number): { row: number, col: number } {
	let row = 0;
	let col = 0;
	let at = 0;
	for (const ch of src) {
		if (at === index) {
			return { row, col };
		}
		if (ch === "\n") {
			row++;
			col = 0;
		}
		else {
			col++;
		}
		at++;
	}
	return { row, col };
}

export function report(diagnostics: Diagnostic[]): "errors" | "no-errors" {
	let result = false;
	for (const diag of diagnostics) {
		console.log(diag.message);
		for (const span of diag.spans) {
			const file = files[span.file];
			let start = getPlace(file.src, span.start);
			let end = getPlace(file.src, span.end);
			const lines = start.row === end.row ? `${start.row + 1}`
				: `${start.row + 1}-${end.row + 1}`;
			console.log(`    ${file.module.join(".")}:${lines}`);
		}
		console.log();
		if (diag.kind === "error") {
			result = true;
		}
	}
	return result ? "errors" : "no-errors";
}
