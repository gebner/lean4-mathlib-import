# Import mathlib into Lean 4

This tool takes the text export of
[mathlib](https://github.com/leanprover-community/mathlib) and creates an olean
file that you can import with [Lean 4](https://github.com/leanprover/lean4).

Notation, etc., is not supported.  All theorems are imported as axioms.  Some
names are adapted to the new convention, some aren't.  **This code is not
intended for production.**

You can build the `Mathlib.olean` as follows (make sure you have enough RAM):
```
make
make emacs    # install the lean4 emacs package yourself!
```
(If you're on NixOS, you might need to run everything inside `nix-shell ~/lean4`.)

After that, you can import mathlib and play with it in Lean 4:
```lean
import Import.Mathlib
#check (deriv : (real → real) → (real → real))
```
