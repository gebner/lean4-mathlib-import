-- new_frontend
import Init.Lean

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

namespace List

def splitAt {α} (xs : List α) (i : Nat) : List α × List α :=
(xs.take i, xs.drop i)

end List

structure ExportParserState :=
(names : Array Name)
(levels : Array Level)
(exprs : Array Expr)
(decls : Array Declaration)

namespace ExportParserState

def initial : ExportParserState := {
  names := #[Name.anonymous],
  levels := #[levelZero],
  exprs := #[],
  decls := #[],
}

instance : Inhabited ExportParserState := ⟨initial⟩

def getExpr! (s : ExportParserState) (i : Nat) : Expr :=
s.exprs.get! i

def getLevel! (s : ExportParserState) (i : Nat) : Level :=
s.levels.get! i

def getName! (s : ExportParserState) (i : Nat) : Name :=
s.names.get! i

def parseBinderInfo : String → BinderInfo
| "#BD" => BinderInfo.default
| "#BI" => BinderInfo.implicit
| "#BS" => BinderInfo.strictImplicit
| "#BC" => BinderInfo.instImplicit
| _ => BinderInfo.default

def parseIntros (s : ExportParserState) : List String → List Constructor
| (n :: t :: is) => { name := s.getName! n.toNat, type := s.getExpr! t.toNat } :: parseIntros is
| _ => []

def processLine (s : ExportParserState) (l : String) : ExportParserState :=
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
| ("#AX" :: n :: t :: ups) =>
  { decls := s.decls.push (Declaration.axiomDecl {
      name := s.getName! n.toNat,
      lparams := ups.map (fun up => s.getName! up.toNat),
      type := s.getExpr! t.toNat,
      isUnsafe := false,
    }), ..s }
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
  let (is, ups) := rest.splitAt (2 * nis.toNat);
  let lparams := ups.map (fun up => s.getName! up.toNat);
  { decls := s.decls.push (Declaration.inductDecl lparams nps.toNat [{
      name := s.getName! n.toNat,
      type := s.getExpr! t.toNat,
      ctors := s.parseIntros is,
    }] false), ..s }
| _ => s

end ExportParserState
