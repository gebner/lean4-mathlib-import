import Init.Lean
import Import.ExportParser
import Import.Import

open Lean Lean.Elab.Command

def handle_import_export_core (stx : Syntax) (fn : String) : CommandElabM Unit := do
f ← liftIO stx (IO.FS.readFile fn);
let lines := f.splitOn "\n";
let parsed := lines.foldl ExportParserState.processLine ExportParserState.initial;
runTermElabM none $ fun _ => do
Lean.Elab.Term.liftMetaM stx $ do
(_, s) ← StateT.run (parsed.decls.mapM ImportState.processDecl) ImportState.default;
pure ()

new_frontend

open Lean Lean.Elab.Command

syntax [importExport] "import_export " str : command

@[commandElab importExport]
def handle_import_export : CommandElab :=
fun (stx : Syntax) => match_syntax stx with
| `(import_export $fn) =>
  handle_import_export_core stx (fn.isStrLit?.getD "not a valid filename")
| _ => throwUnsupportedSyntax

import_export "export.txt"
