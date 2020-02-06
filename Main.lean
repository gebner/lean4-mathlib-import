-- new_frontend
import Init.Lean
import Import.ExportParser
import Import.Import

open Lean

def main (args : List String) : IO UInt32 := do
exportTxt ← IO.FS.readFile (args.get! 0);
let outputOLean := args.get! 1;
let lines := exportTxt.splitOn "\n";
let s := lines.foldl ExportParserState.processLine ExportParserState.initial;
-- env0 ← mkEmptyEnvironment;
path ← IO.getEnv "LEAN_PATH";
IO.println path;
initSearchPath path;
env0 ← importModules [{ module := `Init.Default }];
(_, env) ← IO.runMeta (StateT.run (s.decls.mapM ImportState.processDecl) ImportState.default) env0;
env.displayStats;
writeModule env outputOLean;
pure 0
