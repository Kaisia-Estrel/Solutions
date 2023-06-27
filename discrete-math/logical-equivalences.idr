
import Data.So

-- p ∧ q ≡ q ∧ p
commutative_and : (p,q : Bool) -> p && q = q && p
commutative_and False q = case q of { False => Refl ; True => Refl }
commutative_and True q = case q of { False => Refl ; True => Refl }

-- p ∨ q ≡ q ∨ p
commutative_or : (p,q : Bool) -> p || q = q || p
commutative_or False q = case q of { False => Refl ; True => Refl }
commutative_or True q = case q of { False => Refl ; True => Refl }

-- (p ∧ q) ∧ r ≡ p ∧ (q ∧ r)
associative_and : (p,q,r : Bool) -> (p && q) && r = p && (q && r)
associative_and False q r = Refl
associative_and True q r = Refl

-- (p ∨ q) ∨ r ≡ p ∨ (q ∨ r)
associative_or : (p,q,r : Bool) -> (p || q) || r = p || (q || r)
associative_or False q r = Refl
associative_or True q r = Refl

data Tautology : Bool -> Type where
  Tau : Tautology True

-- p ∧ t ≡ p
identity_tautology : (t : Bool) -> {auto 0 _ : Tautology t} -> (p : Bool) -> p && t = p
identity_tautology True False = Refl
identity_tautology True True = Refl

data Contradiction : Bool -> Type where
  Contra : Contradiction False

-- p ∨ c ≡ p
identity_contradiction : (c : Bool) -> {auto 0 _ : Contradiction c} -> (p : Bool) -> p || c = p
identity_contradiction False False = Refl
identity_contradiction False True = Refl

-- p ∨ ~p ≡ t
negation_tautology : (t : Bool) -> {auto 0 _ : Tautology t} -> (p : Bool) -> p || not p = t
negation_tautology True False = Refl
negation_tautology True True = Refl

-- p ∧ ~p ≡ c
negation_contradiction : (c : Bool) -> {auto 0 _ : Contradiction c} -> (p : Bool) -> p && not p = c
negation_contradiction False False = Refl
negation_contradiction False True = Refl

-- ~(~p) ≡ p
double_negative : (p : Bool) -> not (not p) = p
double_negative False = Refl
double_negative True = Refl

-- p ∧ p ≡ p
idempotent_and : (p : Bool) -> p && p = p
idempotent_and False = Refl
idempotent_and True = Refl

-- p ∨ p ≡ p
idempotent_or : (p : Bool) -> p || p = p
idempotent_or False = Refl
idempotent_or True = Refl

-- p ∨ t ≡ t
universal_bound_or : (p,t : Bool) -> {auto 0 _ : Tautology t} -> p || t = t
universal_bound_or False t = Refl
universal_bound_or True True = Refl

-- p ∧ c ≡ c
universal_bound_and : (p,c : Bool) -> {auto 0 _ : Contradiction c} -> p && c = c
universal_bound_and False False = Refl
universal_bound_and True False = Refl

-- ~(p ∧ q) ≡ ~p ∨ ~q
de_morgen_and_or : (p,q : Bool) -> not (p && q) = not p || not q 
de_morgen_and_or False q = Refl
de_morgen_and_or True q = Refl

-- ~(p ∨ q) ≡ ~p ∧ ~q
de_morgen_or_and : (p,q : Bool) -> not (p || q) = not p && not q
de_morgen_or_and False q = Refl
de_morgen_or_and True q = Refl

-- p ∨ (p ∧ q) ≡ p
absorption_or_and : (p,q : Bool) -> p || (p && q) = p
absorption_or_and False q = Refl
absorption_or_and True q = Refl

-- p ∧ (p ∨ q) ≡ p
absorption_and_or : (p,q : Bool) -> p && (p || q) = p
absorption_and_or False q = Refl
absorption_and_or True q = Refl

-- ~t ≡ c
negation_t : (t,c : Bool) -> {auto 0 _ : Tautology t} -> {auto 0 _ : Contradiction c} -> not t = c
negation_t True False = Refl

-- ~c ≡ t
negation_c : (t,c : Bool) -> {auto 0 _ : Tautology t} -> {auto 0 _ : Contradiction c} -> not c = t
negation_c True False = Refl
