-- new_frontend
import Init.Lean
import Import.ExportParser
import Import.Import

open Lean

def main (args : List String) : IO UInt32 := do
f ← IO.FS.readFile (args.get! 0);
let lines := f.splitOn "\n";
let s := lines.foldl ExportParserState.processLine ExportParserState.initial;
-- env0 ← mkEmptyEnvironment;
path ← IO.getEnv "LEAN_PATH";
initSearchPath path;
env0 ← importModules [⟨`Init.Default, false⟩];
(_, env) ← IO.runMeta (StateT.run (s.decls.mapM ImportState.processDecl) ImportState.default) env0;
env.displayStats;
pure 0
