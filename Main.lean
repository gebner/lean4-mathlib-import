-- new_frontend
import Init.Lean
import Import.ExportParser
import Import.Import

open Lean

def parseExport (fn : String) : IO ExportParserState := do
conts ← IO.FS.readFile fn;
let lines := conts.splitOn "\n";
pure $ lines.foldl ExportParserState.processLine ExportParserState.initial

def parseName (n : String) : Name :=
(n.splitOn ".").foldl mkNameStr Name.anonymous

def parseAttrs (fn : String) : IO (List (Name × Name)) := do
conts ← IO.FS.readFile fn;
let lines := conts.splitOn "\n";
let lines := lines.filter (fun line => line != "");
pure $ lines.map $ fun line =>
  match (line.splitOn " ") with
  | [attr, decl] => (parseName decl, attr)
  | _ => (`has_add, `class) -- dummy

def main (args : List String) : IO UInt32 := do
exportFile ← parseExport (args.get! 0);
attrsFile ← parseAttrs (args.get! 1);
IO.println attrsFile;
let outputOLean := args.get! 2;
-- env0 ← mkEmptyEnvironment;
path ← IO.getEnv "LEAN_PATH";
IO.println path;
initSearchPath path;
env0 ← importModules [{ module := `Init.Default }];
(_, env) ← IO.runMeta (StateT.run (do
  exportFile.decls.mapM ImportState.processDecl;
  attrsFile.mapM (fun ⟨d,a⟩ => ImportState.addAttr d a)) ImportState.default) env0;
env.displayStats;
writeModule env outputOLean;
pure 0
