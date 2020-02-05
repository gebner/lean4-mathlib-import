import Init.Lean
-- import Import.ExportParser

open Lean Lean.Meta

namespace Array

def take {α} (xs : Array α) (i : Nat) : Array α :=
(xs.toList.take i).toArray

def splitAt {α} (xs : Array α) (i : Nat) : Array α × Array α :=
((xs.toList.take i).toArray, (xs.toList.drop i).toArray)

end Array

namespace Lean

-- #print Or.rec

-- def Or.ndrec : ∀ {a b C : Prop}, (a → C) → (b → C) → a ∨ b → C :=
-- fun a b C => @Or.rec a b (fun _ => C)

-- inductive less_than_or_equal (a : Nat) : Nat → Prop
-- | refl : less_than_or_equal a
-- | step : ∀ {b}, less_than_or_equal b → less_than_or_equal (Nat.succ b)

-- #print less_than_or_equal.rec
-- def less_than_or_equal.ndrec : ∀ {a : Nat} {C : Nat → Prop},
--   C a →
--   (∀ {b : Nat}, less_than_or_equal a b → C b → C (Nat.succ b)) →
--   ∀ {a_1 : Nat}, less_than_or_equal a a_1 → C a_1 :=
-- fun a C => @less_than_or_equal.rec a (fun a _ => C a)

def mkOldRecursor (indTy : Name) : MetaM (Option Declaration) := do
some (ConstantInfo.inductInfo indI) ← getConst indTy |
  panic! (toString indTy);
indTy' ← inferType (mkConst indI.name (indI.lparams.map mkLevelParam));
forallTelescopeReducing indTy' $ fun _ indSort => do
Expr.sort level _ ← pure indSort | panic! (toString indSort);
let useDepElim := level.normalize != levelZero;
if useDepElim then pure none else some <$> do
some (ConstantInfo.recInfo recI) ← getConst (indTy ++ "rec") |
  panic! (toString $ indTy ++ "rec");
let rec := mkConst recI.name (recI.lparams.map mkLevelParam);
recTy ← inferType rec;
forallTelescopeReducing recTy $ fun args _ => do
-- Meta.dbgTrace args;
let (params, args) := args.splitAt recI.nparams;
let (motive, args) := args.splitAt 1;
let motive := motive.get! 0;
motiveTy ← inferType motive;
forallTelescopeReducing motiveTy $ fun _ elimSort => do
Expr.sort elimLevel _ ← pure elimSort | panic! (toString elimSort);
let (minorPremises, args) := args.splitAt recI.nminors;
let (indices, major) := args.splitAt recI.nindices;
let majorPremise := major.get! 0;
-- Meta.dbgTrace (params, motive, indices, minorPremises, majorPremise);
oldMotiveTy ← Meta.mkForall indices (mkSort elimLevel);
withLocalDecl `C oldMotiveTy BinderInfo.implicit $ fun oldMotive => do
newMotive ← Meta.mkLambda (indices.push majorPremise) (mkAppN oldMotive indices);
val ← Meta.mkLambda ((params).push oldMotive) $ mkAppN rec ((params).push newMotive);
-- Meta.dbgTrace val;
ty ← inferType val;
-- Meta.dbgTrace ty;
pure $ Declaration.defnDecl {
  name := indTy ++ "meinNdRec",
  lparams := recI.lparams,
  type := ty,
  value := val,
  isUnsafe := false,
  hints := ReducibilityHints.regular 0,
}

-- #eval do mkOldRecursor `Eq; pure ()

-- #print Eq.rec
-- #print prefix Eq

-- let (params, indices) := paramsIndices.splitAt indI.nparams;
-- let useDepElim := !level.normalize.isZero;
-- if useDepElim then pure none else some <$> do
-- let elimIntoProp := true; -- FIXME
-- elimLevel ← if elimIntoProp then pure levelZero else mkLevelParam <$> mkFreshId;
-- oldMotiveTy ← Meta.mkForall params (mkSort elimLevel);
-- withLocalDecl `C oldMotiveTy BinderInfo.default $ fun oldMotive =>

-- #eval do Declaration.defnDecl v ← mkOldRecursor `Or | panic! "2"; pure v.type

end Lean
