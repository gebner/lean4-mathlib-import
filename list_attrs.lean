import all

/-
Lean 3 script to dump attributes
-/

open tactic

#eval do
let as := [`class, `instance, `simp],
as.mmap' $ λ a, do
ns ← attribute.get_instances a,
ns.mmap' $ λ n, do
trace $ to_string a ++ " " ++ to_string n
