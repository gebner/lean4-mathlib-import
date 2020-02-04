-- new_frontend
import Init.Lean

namespace Lean.Declaration

def names : Lean.Declaration → List Lean.Name
| axiomDecl v => [v.name]
| defnDecl v => [v.name]
| thmDecl v => [v.name]
| opaqueDecl v => [v.name]
| quotDecl => []
| mutualDefnDecl vs => vs.map (fun v => v.name)
| inductDecl _ _ is _ => is.map (fun i => i.name)

end Lean.Declaration

open Lean

namespace Array

partial def write {α} [Inhabited α] : ∀ (arr : Array α) (i : Nat) (x : α), Array α
| arr, i, x =>
if i = arr.size then
  arr.push x
else if i > arr.size then
  write (arr.push (Inhabited.default _)) i x
else
  arr.set! i x

end Array

structure ImportState :=
(names : Array Name)
(levels : Array Level)
(exprs : Array Expr)
(decls : Array Declaration)

namespace ImportState

def initial : ImportState := {
  names := #[Name.anonymous],
  levels := #[levelZero],
  exprs := #[],
  decls := #[],
}

instance : Inhabited ImportState := ⟨initial⟩

def getExpr! (s : ImportState) (i : Nat) : Expr :=
s.exprs.get! i

def getLevel! (s : ImportState) (i : Nat) : Level :=
s.levels.get! i

def getName! (s : ImportState) (i : Nat) : Name :=
s.names.get! i

def parseBinderInfo : String → BinderInfo
| "#BD" => BinderInfo.default
| "#BI" => BinderInfo.implicit
| "#BS" => BinderInfo.strictImplicit
| "#BC" => BinderInfo.instImplicit
| _ => BinderInfo.default

def parseIntros (s : ImportState) : List String → List Constructor
| (n :: t :: is) => { name := s.getName! n.toNat, type := s.getExpr! t.toNat } :: parseIntros is
| _ => []

def processLine (s : ImportState) (l : String) : ImportState :=
match l.splitOn " " with
| (i :: "#NS" :: j :: rest) =>
  { names := s.names.write i.toNat (mkNameStr (s.getName! j.toNat) (" ".intercalate rest)), ..s}
| [i, "#NI", j, k] =>
  { names := s.names.write i.toNat (mkNameNum (s.getName! j.toNat) k.toNat), ..s}
| [i, "#US", j] =>
  { levels := s.levels.write i.toNat (mkLevelSucc (s.getLevel! j.toNat)), ..s }
| [i, "#UM", j1, j2] =>
  { levels := s.levels.write i.toNat (mkLevelMax (s.getLevel! j1.toNat) (s.getLevel! j2.toNat)), ..s }
| [i, "#UIM", j1, j2] =>
  { levels := s.levels.write i.toNat (mkLevelIMax (s.getLevel! j1.toNat) (s.getLevel! j2.toNat)), ..s }
| [i, "#UP", j] =>
  { levels := s.levels.write i.toNat (mkLevelParam (s.getName! j.toNat)), ..s }
| [i, "#EV", j] =>
  { exprs := s.exprs.write i.toNat (mkBVar j.toNat), ..s }
| [i, "#ES", j] =>
  { exprs := s.exprs.write i.toNat (mkSort (s.getLevel! j.toNat)), ..s }
| (i :: "#EC" :: j :: us) =>
  { exprs := s.exprs.write i.toNat (mkConst (s.getName! j.toNat) (us.map (fun u => s.getLevel! u.toNat))), ..s }
| [i, "#EA", j1, j2] =>
  { exprs := s.exprs.write i.toNat (mkApp (s.getExpr! j1.toNat) (s.getExpr! j2.toNat)), ..s }
| [i, "#EL", bi, j1, j2, j3] =>
  { exprs := s.exprs.write i.toNat (mkLambda (s.getName! j1.toNat) (parseBinderInfo bi) (s.getExpr! j2.toNat) (s.getExpr! j3.toNat)), ..s }
| [i, "#EP", bi, j1, j2, j3] =>
  { exprs := s.exprs.write i.toNat (mkForall (s.getName! j1.toNat) (parseBinderInfo bi) (s.getExpr! j2.toNat) (s.getExpr! j3.toNat)), ..s }
| [i, "#EZ", j1, j2, j3, j4] =>
  { exprs := s.exprs.write i.toNat (mkLet (s.getName! j1.toNat) (s.getExpr! j2.toNat) (s.getExpr! j3.toNat) (s.getExpr! j4.toNat)), ..s }
| ("#DEF" :: n :: t :: v :: ups) =>
  { decls := s.decls.push (Declaration.defnDecl {
      name := s.getName! n.toNat,
      lparams := ups.map (fun up => s.getName! up.toNat),
      type := s.getExpr! t.toNat,
      value := s.getExpr! v.toNat,
      isUnsafe := false,
      hints := ReducibilityHints.regular 0,
    }), ..s }
| ("#IND" :: nps :: n :: t :: nis :: rest) =>
  let is := rest.take (2 * nis.toNat);
  let ups := rest.drop (2 * nis.toNat);
  let lparams := ups.map (fun up => s.getName! up.toNat);
  { decls := s.decls.push (Declaration.inductDecl lparams nps.toNat [{
      name := s.getName! n.toNat,
      type := s.getExpr! t.toNat,
      ctors := s.parseIntros is,
    }] false), ..s }
| _ => s

end ImportState

#print Or.rec
#print Exists
#print Exists.rec

def main (args : List String) : IO UInt32 := do
f ← IO.FS.readFile (args.get! 0);
let lines := f.splitOn "\n";
let s := lines.foldl ImportState.processLine ImportState.initial;
env0 ← mkEmptyEnvironment;
let env := s.decls.foldl (fun env decl =>
  match Environment.addDecl env decl with
  | Except.error (err : KernelException) =>
    @panic _ ⟨env⟩ (toString (fmt (decl.names, err.toMessageData (Inhabited.default Options))))
  | Except.ok env' => env') env0;
env.displayStats;
pure 0
