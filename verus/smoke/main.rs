//! A smoke proof for the Verus wiring.
//!
//! The lemma is deliberately trivial: it needs no axioms and no induction, so
//! a failure points at the prover installation or invocation rather than at
//! the proof itself. Once the scope-resolution kernel is proved, its proof
//! file replaces or extends this one and `VERUS_FLAGS` follows.

use vstd::prelude::*;

verus! {

/// Establish that natural-number addition commutes.
///
/// Z3 discharges this from linear arithmetic alone; the lemma exists so
/// `make verus` exercises a real verification query.
proof fn addition_commutes(left: nat, right: nat)
    ensures left + right == right + left
{
}

fn main() {
    proof {
        addition_commutes(2, 3);
    }
}

} // verus!