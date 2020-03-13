import Import.OldRecursor
import Import.ExportParser

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

unsafe def Lean.Environment.addAttribute'' (env : Environment) (declName : Name) (attrName : Name) (args : Syntax := Syntax.missing) (persistent := true) : Except IO.Error Environment :=
unsafeIO $ env.addAttribute declName attrName args persistent

@[implementedBy Lean.Environment.addAttribute'']
constant Lean.Environment.addAttribute' (env : Environment) (declName : Name) (attrName : Name) (args : Syntax := Syntax.missing) (persistent := true) : Except IO.Error Environment :=
arbitrary _

namespace Lean.Expr
open Lean.Expr

def replaceConstNames (f : Name → Name) (e : Expr) : Expr :=
e.replace $ fun e => match e with
| e@(const n us _) => if f n == n then none else mkConst (f n) us
| _ => none

end Lean.Expr

structure ImportState :=
(newNames : HashMap Name Name)
(ignored : HashSet Name)

namespace ImportState

def align (s : ImportState) (f t : Name) : ImportState :=
{ newNames := s.newNames.insert f t, ignored := s.ignored.insert f }

def unchanged (s : ImportState) (n : Name) : ImportState :=
s.align n n

def rename (s : ImportState) (f t : Name) : ImportState :=
{ newNames := s.newNames.insert f t, ..s }

def default : ImportState :=
let s : ImportState := { newNames := HashMap.empty, ignored := HashSet.empty };
let s := s.align `or `Or;
let s := s.align `or.inl `Or.inl;
let s := s.align `or.inr `Or.inr;
let s := s.align `or.rec `Or.rec;
let s := s.align `and `And;
let s := s.align `and.intro `And.intro;
let s := s.unchanged `bit0;
let s := s.unchanged `bit1;
let s := s.rename `quot `Quot;
let s := s.rename `quot.mk `Quot.mk;
let s := s.rename `quot.lift `Quot.lift;
let s := s.rename `quot.ind `Quot.ind;
let s := s.unchanged `propext;
let s := s.unchanged `funext;
let s := s.align `has_add `HasAdd;
let s := s.align `has_add.rec `HasAdd.rec;
let s := s.align `has_add.mk `HasAdd.mk;
let s := s.align `has_add.add `HasAdd.add;
let s := s.align `has_one `HasOne;
let s := s.align `has_one.rec `HasOne.rec;
let s := s.align `has_one.mk `HasOne.mk;
let s := s.align `has_one.one `HasOne.one;
let s := s.align `has_zero `HasZero;
let s := s.align `has_zero.rec `HasZero.rec;
let s := s.align `has_zero.mk `HasZero.mk;
let s := s.align `has_zero.zero `HasZero.zero;
let s := s.align `not `Not;
let s := s.rename `dite `dite3;
let s := s.align `decidable `Decidable;
let s := s.align `decidable.rec `Decidable.rec;
let s := s.align `decidable.is_false `Decidable.isFalse;
let s := s.align `decidable.is_true `Decidable.isTrue;
let s := s.align `eq `Eq;
let s := s.align `eq.refl `Eq.refl;
let s := s.unchanged `rfl;
let s := s.align `false `False;
let s := s.unchanged `absurd;
let s := s.unchanged `congr;
let s := s.rename `ite `ite3;
let s := s.align `bool `Bool;
let s := s.align `bool.ff `Bool.false;
let s := s.align `bool.tt `Bool.true;
let s := s.align `bool.rec `Bool.rec;
let s := s.align `true `True;
let s := s.align `true.intro `True.intro;
let s := s.unchanged `trivial;
let s := s.unchanged `id;
let s := s.unchanged `id.def;
let s := s.unchanged `default.sizeof;
let s := s.align `nat `Nat;
let s := s.align `nat.zero `Nat.zero;
let s := s.align `nat.rec `Nat.rec;
let s := s.align `nat.succ `Nat.succ;
let s := s.align `iff `Iff;
let s := s.align `iff.intro `Iff.intro;
let s := s.unchanged `Exists;
let s := s.unchanged `cond;
let s := s.unchanged `mt;
let s := s.rename `measure `measure3;
-- let s := s.rename `measure_wf `measure_wf3;
let s := s.rename `coe `coe3;
let s := s.rename `failure `failure3;
let s := s.rename `timeit `timeit3;
let s := s.align `inhabited `Inhabited;
let s := s.align `inhabited.default `Inhabited.default;
let s := s.align `inhabited.mk `Inhabited.mk;
let s := s.align `inhabited.rec `Inhabited.rec;
let s := s.align `nonempty `Nonempty;
let s := s.align `nonempty.intro `Nonempty.intro;
let s := s.align `nonempty.rec `Nonempty.rec;
let s := s.align `classical.choice `Classical.choice;
let s := s.align `default `Inhabited.default;
let s := s.unchanged `arbitrary;
let s := s.align `xor `Xor;
let s := s.unchanged `cast;
let s := s.rename `guard `guard3;
let s := s.rename `guardb `guardb3;
let s := s.rename `when `when3;
let s := s.rename `modify `modify3;
let s := s.rename `assert `assert3;
let s := s.rename `lift `lift3;
let s := s.unchanged `std.prec.arrow;
let s := s.unchanged `std.prec.default;
let s := s.unchanged `std.prec.max;
let s := s.unchanged `std.priority.max;
let s := s.unchanged `std.priority.default;
let s := s.unchanged `implies;
let s := s.unchanged `implies.trans;
let s := s.unchanged `flip;
let s := s.rename `optional `optional3;
s

def max (a b : UInt32) : UInt32 :=
if a >= b then a else b

def getMaxHeight (env : Environment) : Expr → UInt32
| Expr.const n ls _ =>
  match env.find? n with
  | some (ConstantInfo.defnInfo { hints := ReducibilityHints.regular d, .. }) => d
  | _ => 0
| Expr.lam n d b _ => max (getMaxHeight d) (getMaxHeight b)
| Expr.forallE n d b _ => max (getMaxHeight d) (getMaxHeight b)
| Expr.letE n d v b _ => max (max (getMaxHeight d) (getMaxHeight v)) (getMaxHeight b)
| Expr.app a b _ => max (getMaxHeight a) (getMaxHeight b)
| Expr.mdata _ a _ => getMaxHeight a
| Expr.proj _ _ a _ => getMaxHeight a
| _ => 0

-- TODO FIXME TODO
private def myAddDecl (env : Environment) (decl : Declaration) : Environment :=
match Environment.addDecl env decl with
| Except.error (err : KernelException) =>
  @panic _ ⟨env⟩ (toString (fmt (decl.names, err.toMessageData (Inhabited.default Options))))
| Except.ok env' => env'

def addDecl (decl : Declaration) : MetaM Unit :=
modify $ fun s => { env := myAddDecl s.env decl, ..s }

def addAttr' (decl : Name) (attr : Name) : MetaM Unit :=
modify $ fun s => {
  env :=
    match s.env.addAttribute' decl attr with
    | Except.error err => @panic _ ⟨s.env⟩ (toString err)
    | Except.ok env => env,
  ..s }

def processDecl (d : Declaration) : StateT ImportState MetaM Unit := do
s ← StateT.get;
let f := fun n => s.newNames.findD n n;
let r := fun e => Lean.Expr.replaceConstNames f e;
match d with
| Declaration.axiomDecl defn => do
  StateT.lift $ Meta.dbgTrace defn.name;
  if s.ignored.contains defn.name then pure () else
  StateT.lift $ addDecl $ Declaration.axiomDecl {
    name := f defn.name,
    type := r defn.type,
    ..defn
  }
| Declaration.defnDecl defn => do
  StateT.lift $ Meta.dbgTrace defn.name;
  isProp ← StateT.lift $ Meta.isProp (r defn.type);
  if s.ignored.contains defn.name then pure ()
  else if defn.name == `free_abelian_group.semigroup._proof_1 || defn.name == `std.priority.max.equations._eqn_1 then
    StateT.lift $ addDecl $ Declaration.axiomDecl {
      name := f defn.name,
      type := r defn.type,
      ..defn
    }
  else if isProp then
    StateT.lift $ addDecl $ Declaration.thmDecl {
      name := f defn.name,
      type := r defn.type,
      value := Task.pure $ r defn.value,
      ..defn
    }
  else do
    env ← liftM Meta.getEnv;
    StateT.lift $ addDecl $ Declaration.defnDecl {
      name := f defn.name,
      type := r defn.type,
      value := r defn.value,
      hints := ReducibilityHints.regular (getMaxHeight env (r defn.value) + 1),
      ..defn
    }
| Declaration.inductDecl lps nps [ind] iu => do
  StateT.lift $ Meta.dbgTrace ind.name;
  -- StateT.lift $ Meta.dbgTrace (ind.name, s.ignored.fold (flip List.cons) []);
  if s.ignored.contains ind.name then pure () else
  StateT.lift $ addDecl $ Declaration.inductDecl lps nps
    [{ name := f ind.name,
       type := r ind.type,
       ctors := ind.ctors.map (fun ctor => { name := f ctor.name, type := r ctor.type, ..ctor }),
       ..ind }]
    iu;
   oldRec ← StateT.lift $ mkOldRecursor (f ind.name);
   match oldRec with
   | some oldRec => do
     StateT.lift $ addDecl oldRec;
     -- StateT.lift $ Meta.dbgTrace (ind.name ++ "rec", oldRec.names.get! 0);
     modify $ fun s => { newNames := s.newNames.insert (ind.name ++ "rec") (oldRec.names.get! 0), ..s }
   | _ => pure ();
   pure ()
| _ => panic! (toString d.names)

def addAttr (decl attr : Name) : StateT ImportState MetaM Unit := do
s ← StateT.get;
let decl := s.newNames.findD decl decl;
env ← StateT.lift $ Meta.getEnv;
when (env.contains decl) $
when (attr == `class || attr == `instance) $ do
liftM $ Meta.dbgTrace (decl, attr);
liftM $ addAttr' decl attr

end ImportState
