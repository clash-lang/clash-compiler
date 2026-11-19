-------------------------------------------------------------------------------
-- Copyright  :  © 2026, Felix Klein
-- License    :  BSD2 (see the file LICENSE)
-- Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>
--
-- Proven properties of the `Integral Period` instance in
-- `Clash.Signal.Internal`.
-------------------------------------------------------------------------------

open import Function.Base using (_$_)
open import Data.Sum.Base using (inj₂)
open import Data.Sign.Base as Sign using ()
open import Data.Nat.Base as ℕ using (ℕ; suc)
open import Data.Nat.Properties as ℕ using ()
open import Data.Nat.GCD as ℕ using ()
open import Data.Nat.DivMod as ℕ using (/-congʳ; /-congˡ; m*n/m*o≡n/o)
open import Data.Integer.Base as ℤ using (ℤ; 1ℤ; +_; -[1+_]; +[1+_]; +0; ∣_∣)
open import Data.Integer.Properties as ℤ using ()
open import Data.Integer.GCD as ℤ using (gcd; gcd-zeroʳ)
open import Data.Integer.DivMod using (a≡a%ℕn+[a/ℕn]*n; div-neg-is-neg-/ℕ; n%d<d)
open import Data.Rational as ℚ hiding (∣_∣)
open import Data.Rational.Properties
open import Relation.Binary.PropositionalEquality.Core using (_≡_)
open import Relation.Binary.PropositionalEquality

-------------------------------------------------------------------------------
-- Common Properties of Rationals
-------------------------------------------------------------------------------
--
--  The following are common properties over rationals, which however are
--  not part of the used `agda-stdlib` v2.3. A PR to add them has been
--  accepted upstream (https://github.com/agda/agda-stdlib/pull/2996), which
--  is why they can be assumed obsolete when using `agda-stlib` > v2.4.
--
-------------------------------------------------------------------------------

↥[i/1]≡i : (i : ℤ) → ↥ (i / 1) ≡ i
↥[i/1]≡i i = begin
    ↥ (i / 1)              ≡⟨ ℤ.*-identityʳ (↥ (i / 1)) ⟨
    ↥ (i / 1) ℤ.* 1ℤ       ≡⟨ cong (↥ (i / 1) ℤ.*_) $ gcd-zeroʳ i ⟨
    ↥ (i / 1) ℤ.* gcd i 1ℤ ≡⟨ ↥-/ i 1 ⟩
    i                      ∎
  where open ≡-Reasoning


↧ₙ[i/1]≡1 : (i : ℤ) → ↧ₙ (i / 1) ≡ 1
↧ₙ[i/1]≡1 i = ℤ.+-injective $ begin
    ↧ (i / 1)               ≡⟨ ℤ.*-identityʳ (↧ (i / 1)) ⟨
    ↧ (i / 1) ℤ.* 1ℤ        ≡⟨ cong (↧ (i / 1) ℤ.*_) $ gcd-zeroʳ i ⟨
    ↧ (i / 1) ℤ.* gcd i 1ℤ  ≡⟨ ↧-/ i 1 ⟩
    1ℤ                      ∎
  where open ≡-Reasoning

n/n≡1 : ∀ (n : ℕ) .{{_ : ℕ.NonZero n}} → + n / n ≡ 1ℚ
n/n≡1 n {{nz}} = mkℚ+-cong n/gcd[n,n]≡1 n/gcd[n,n]≡1
  where
  instance g≢0   = ℕ.≢-nonZero (ℕ.gcd[m,n]≢0 n n (inj₂ (ℕ.≢-nonZero⁻¹ n)))
           n/g≢0 = ℕ.≢-nonZero (ℕ.n/gcd[m,n]≢0 n n {{gcd≢0 = g≢0}})

  gcd[n,n]≡n : ∀ n → ℕ.gcd n n ≡ n
  gcd[n,n]≡n n = begin
    ℕ.gcd n n                 ≡⟨ cong₂ ℕ.gcd n*1≡n n*1≡n ⟨
    ℕ.gcd (n ℕ.* 1) (n ℕ.* 1) ≡⟨ ℕ.c*gcd[m,n]≡gcd[cm,cn] n 1 1 ⟨
    n ℕ.* ℕ.gcd 1 1           ≡⟨ n*1≡n ⟩
    n                         ∎
    where
    open ≡-Reasoning
    n*1≡n = ℕ.*-identityʳ n

  n/gcd[n,n]≡1
    = trans (ℕ./-congʳ {ℕ.gcd n n} (gcd[n,n]≡n n)) (ℕ.n/n≡1 n {{nz}})

-i/n≡-[i/n] : ∀ (i : ℤ) (n : ℕ) .{{_ : ℕ.NonZero n}} →
              ℤ.- i / n ≡ - (i / n)
-i/n≡-[i/n] +0       n = trans (0/n≡0 n) (cong -_ (sym (0/n≡0 n)))
-i/n≡-[i/n] +[1+ m ] n = refl
-i/n≡-[i/n] -[1+ m ] n
  with +[1+ m ] / n
... | mkℚ -[1+ a ] d prf = refl
... | mkℚ +0       d prf = refl
... | mkℚ +[1+ a ] d prf = refl

*-cancelˡ-/ : ∀ p {q r} .{{_ : ℕ.NonZero r}} .{{_ : ℕ.NonZero (p ℕ.* r)}} →
              (+ p ℤ.* q) / (p ℕ.* r) ≡ q / r
*-cancelˡ-/ p {q} {r} = proof q
  where
  open ≡-Reasoning

  *-cancelˡ-/-helper : ∀ qₙ → normalize (p ℕ.* qₙ) (p ℕ.* r) ≡ + qₙ / r
  *-cancelˡ-/-helper qₙ = mkℚ+-cong (lemma qₙ) (lemma r)
    where
    instance
      p≢0    = ℕ.m*n≢0⇒m≢0 p
      g≢0    = ℕ.≢-nonZero $ ℕ.gcd[m,n]≢0 (p ℕ.* qₙ) (p ℕ.* r) $ inj₂
                           $ ℕ.≢-nonZero⁻¹ $ p ℕ.* r
      n/g≢0  = ℕ.≢-nonZero $ ℕ.n/gcd[m,n]≢0 (p ℕ.* qₙ) (p ℕ.* r) {{gcd≢0 = g≢0}}
      g≢0'   = ℕ.≢-nonZero $ ℕ.gcd[m,n]≢0 qₙ r $ inj₂ $ ℕ.≢-nonZero⁻¹ r
      n/g≢0' = ℕ.≢-nonZero $ ℕ.n/gcd[m,n]≢0 qₙ r {{gcd≢0 = g≢0'}}
      p*g≢0  = ℕ.m*n≢0 p (ℕ.gcd qₙ r)

    lemma : ∀ n → (p ℕ.* n) ℕ./ ℕ.gcd (p ℕ.* qₙ) (p ℕ.* r) ≡ n ℕ./ ℕ.gcd qₙ r
    lemma n = begin
      p ℕ.* n ℕ./ ℕ.gcd (p ℕ.* qₙ) (p ℕ.* r)
        ≡⟨ ℕ./-congʳ $ ℕ.c*gcd[m,n]≡gcd[cm,cn] p qₙ r ⟨
      p ℕ.* n ℕ./ (p ℕ.* ℕ.gcd qₙ r)
        ≡⟨ ℕ.m*n/m*o≡n/o p n $ ℕ.gcd qₙ r ⟩
      n ℕ./ ℕ.gcd qₙ r
        ∎

  proof : ∀ q → (+ p ℤ.* q) / (p ℕ.* r) ≡ q / r
  proof (+ qₙ) = begin
    + p ℤ.* + qₙ / (p ℕ.* r) ≡⟨ /-cong (ℤ.pos-* p qₙ) refl ⟨
    + (p ℕ.* qₙ) / (p ℕ.* r) ≡⟨ *-cancelˡ-/-helper qₙ ⟩
    + qₙ / r                 ∎
  proof -[1+ qₙ ] = begin
    + p ℤ.* -[1+ qₙ ] / (p ℕ.* r)
      ≡⟨ /-cong (ℤ.neg-distribʳ-* (+ p) +[1+ qₙ ]) refl ⟨
    ℤ.- (Sign.+ ℤ.◃ p ℕ.* suc qₙ) / (p ℕ.* r)
      ≡⟨ /-cong (cong (ℤ.-_) (ℤ.pos-* p (suc qₙ))) refl ⟨
    ℤ.- + (p ℕ.* suc qₙ) / (p ℕ.* r)
      ≡⟨ -i/n≡-[i/n] (+ (p ℕ.* suc qₙ)) (p ℕ.* r) ⟩
    - (+ (p ℕ.* suc qₙ) / (p ℕ.* r))
      ≡⟨ cong (-_) $ *-cancelˡ-/-helper $ suc qₙ ⟩
    -[1+ qₙ ] / r
      ∎

*-cancelʳ-/ : ∀ p {q r} .{{_ : ℕ.NonZero r}} .{{_ : ℕ.NonZero (r ℕ.* p)}} →
              (q ℤ.* + p) / (r ℕ.* p) ≡ q / r
*-cancelʳ-/ p {q} {r} = begin
   q ℤ.* + p / (r ℕ.* p) ≡⟨ /-cong {q ℤ.* + p} refl (ℕ.*-comm r p) ⟩
   q ℤ.* + p / (p ℕ.* r) ≡⟨ /-cong (ℤ.*-comm q (+ p)) refl ⟩
   + p ℤ.* q / (p ℕ.* r) ≡⟨ *-cancelˡ-/ p ⟩
   q / r                 ∎
   where
   open ≡-Reasoning
   instance p≢0 : ℕ.NonZero p
            p≢0 = ℕ.m*n≢0⇒n≢0 r
            p*r≢0 : ℕ.NonZero (p ℕ.* r)
            p*r≢0 = ℕ.m*n≢0 p r

i/n+j/n≡[i+j]/n : ∀ (i j : ℤ) (n : ℕ) .{{_ : ℕ.NonZero n }} →
                  i / n + j / n ≡ (i ℤ.+ j) / n
i/n+j/n≡[i+j]/n i j n = begin
  i / n + j / n
    ≡⟨ +-def ⟩
  (↥ pᵢ ℤ.* ↧ qⱼ ℤ.+ ↥ qⱼ ℤ.* ↧ pᵢ) / (↧ₙ pᵢ ℕ.* ↧ₙ qⱼ)
    ≡⟨ *-cancelʳ-/ gcd[j,n]ₙ
                   {↥ pᵢ ℤ.* ↧ qⱼ ℤ.+ ↥ qⱼ ℤ.* ↧ pᵢ}
                   { ↧ₙ pᵢ ℕ.* ↧ₙ qⱼ }
     ⟨
  (↥ pᵢ ℤ.* ↧ qⱼ ℤ.+ ↥ qⱼ ℤ.* ↧ pᵢ) ℤ.* gcd[j,n]
    / (↧ₙ pᵢ ℕ.* ↧ₙ qⱼ ℕ.* gcd[j,n]ₙ)
    ≡⟨ *-cancelʳ-/ gcd[i,n]ₙ
                   { (↥ pᵢ ℤ.* ↧ qⱼ ℤ.+ ↥ qⱼ ℤ.* ↧ pᵢ) ℤ.* gcd[j,n] }
                   { ↧ₙ pᵢ ℕ.* ↧ₙ qⱼ ℕ.* gcd[j,n]ₙ }
     ⟨
  (↥ pᵢ ℤ.* ↧ qⱼ ℤ.+ ↥ qⱼ ℤ.* ↧ pᵢ) ℤ.* gcd[j,n] ℤ.* gcd[i,n]
    / (↧ₙ pᵢ ℕ.* ↧ₙ qⱼ ℕ.* gcd[j,n]ₙ ℕ.* gcd[i,n]ₙ)
    ≡⟨ /-cong ↥≡ ↧≡ ⟩
  (i ℤ.+ j) ℤ.* + n / (n ℕ.* n)
    ≡⟨ *-cancelʳ-/ n {i ℤ.+ j} {n} ⟩
  (i ℤ.+ j) / n
    ∎
  where
  open ≡-Reasoning

  pᵢ = i / n
  qⱼ = j / n
  gcd[i,n]ₙ = ℕ.gcd ℤ.∣ i ∣ n
  gcd[i,n]  = + gcd[i,n]ₙ
  gcd[j,n]ₙ = ℕ.gcd ℤ.∣ j ∣ n
  gcd[j,n]  = + gcd[j,n]ₙ

  instance
    _ = ℕ.≢-nonZero $ ℕ.gcd[m,n]≢0 ℤ.∣ i ∣ n $ inj₂ $ ℕ.≢-nonZero⁻¹ n
    _ = ℕ.≢-nonZero $ ℕ.gcd[m,n]≢0 ℤ.∣ j ∣ n $ inj₂ $ ℕ.≢-nonZero⁻¹ n
    _ = ℕ.m*n≢0 (↧ₙ pᵢ ℕ.* ↧ₙ qⱼ) gcd[j,n]ₙ
    _ = ℕ.m*n≢0 (↧ₙ pᵢ ℕ.* ↧ₙ qⱼ ℕ.* gcd[j,n]ₙ) gcd[i,n]ₙ
    _ = ℕ.m*n≢0 n n

  +-def : pᵢ + qⱼ ≡ (↥ pᵢ ℤ.* ↧ qⱼ ℤ.+ ↥ qⱼ ℤ.* ↧ pᵢ) / (↧ₙ pᵢ ℕ.* ↧ₙ qⱼ)
  +-def with record{} ← pᵢ with record{} ← qⱼ = refl

  ↥≡ : (↥ pᵢ ℤ.* ↧ qⱼ ℤ.+ ↥ qⱼ ℤ.* ↧ pᵢ) ℤ.* gcd[j,n] ℤ.* gcd[i,n]
     ≡ (i ℤ.+ j) ℤ.* + n
  ↥≡ = begin
    (↥ pᵢ ℤ.* ↧ qⱼ ℤ.+ ↥ qⱼ ℤ.* ↧ pᵢ) ℤ.* gcd[j,n] ℤ.* gcd[i,n]
      ≡⟨ cong (ℤ._* gcd[i,n])
       $ ℤ.*-distribʳ-+ gcd[j,n]
           (↥ pᵢ ℤ.* ↧ qⱼ)
           (↥ qⱼ ℤ.* ↧ pᵢ)
       ⟩
    (↥ pᵢ ℤ.* ↧ qⱼ ℤ.* gcd[j,n] ℤ.+ ↥ qⱼ ℤ.* ↧ pᵢ ℤ.* gcd[j,n]) ℤ.* gcd[i,n]
      ≡⟨ ℤ.*-distribʳ-+ gcd[i,n]
           (↥ pᵢ ℤ.* ↧ qⱼ ℤ.* gcd[j,n])
           (↥ qⱼ ℤ.* ↧ pᵢ ℤ.* gcd[j,n])
       ⟩
    ↥ pᵢ ℤ.* ↧ qⱼ ℤ.* gcd[j,n] ℤ.* gcd[i,n] ℤ.+
    ↥ qⱼ ℤ.* ↧ pᵢ ℤ.* gcd[j,n] ℤ.* gcd[i,n]
      ≡⟨ cong (ℤ._+ ↥ qⱼ ℤ.* ↧ pᵢ ℤ.* gcd[j,n] ℤ.* gcd[i,n])
       $ cong (ℤ._* gcd[i,n])
       $ ℤ.*-assoc (↥ pᵢ) (↧ qⱼ) gcd[j,n]
       ⟩
    ↥ pᵢ ℤ.* (↧ qⱼ ℤ.* gcd[j,n]) ℤ.* gcd[i,n] ℤ.+
    ↥ qⱼ ℤ.* ↧ pᵢ ℤ.* gcd[j,n] ℤ.* gcd[i,n]
      ≡⟨ cong (ℤ._+ (↥ qⱼ ℤ.* ↧ pᵢ ℤ.* gcd[j,n] ℤ.* gcd[i,n]))
       $ cong (ℤ._* gcd[i,n])
       $ cong (↥ pᵢ ℤ.*_)
       $ ↧-/ j n
       ⟩
    ↥ pᵢ ℤ.* + n ℤ.* gcd[i,n] ℤ.+
    ↥ qⱼ ℤ.* ↧ pᵢ ℤ.* gcd[j,n] ℤ.* gcd[i,n]
      ≡⟨ cong (ℤ._+_ (↥ pᵢ ℤ.* + n ℤ.* gcd[i,n]))
       $ cong (ℤ._* gcd[i,n])
       $ ℤ.*-assoc (↥ qⱼ) (↧ pᵢ) gcd[j,n]
       ⟩
    ↥ pᵢ ℤ.* + n ℤ.* gcd[i,n] ℤ.+
    ↥ qⱼ ℤ.* (↧ pᵢ ℤ.* gcd[j,n]) ℤ.* gcd[i,n]
      ≡⟨ cong (ℤ._+_ (↥ pᵢ ℤ.* + n ℤ.* gcd[i,n]))
       $ cong (ℤ._* gcd[i,n])
       $ cong (↥ qⱼ ℤ.*_)
       $ ℤ.*-comm (↧ pᵢ) gcd[j,n]
       ⟩
    ↥ pᵢ ℤ.* + n ℤ.* gcd[i,n] ℤ.+
    ↥ qⱼ ℤ.* (gcd[j,n] ℤ.* ↧ pᵢ) ℤ.* gcd[i,n]
      ≡⟨ cong (ℤ._+_ (↥ pᵢ ℤ.* + n ℤ.* gcd[i,n]))
       $ cong (ℤ._* gcd[i,n])
       $ ℤ.*-assoc (↥ qⱼ) gcd[j,n] (↧ pᵢ)
       ⟨
    ↥ pᵢ ℤ.* + n ℤ.* gcd[i,n] ℤ.+
    ↥ qⱼ ℤ.* gcd[j,n] ℤ.* ↧ pᵢ ℤ.* gcd[i,n]
      ≡⟨ cong (ℤ._+_ (↥ pᵢ ℤ.* + n ℤ.* gcd[i,n]))
       $ cong (ℤ._* gcd[i,n])
       $ cong (ℤ._* ↧ pᵢ)
       $ ↥-/ j n
       ⟩
    ↥ pᵢ ℤ.* + n ℤ.* gcd[i,n] ℤ.+ j ℤ.* ↧ pᵢ ℤ.* gcd[i,n]
      ≡⟨ cong (ℤ._+_ (↥ pᵢ ℤ.* + n ℤ.* gcd[i,n]))
       $ ℤ.*-assoc j (↧ pᵢ) gcd[i,n]
       ⟩
    ↥ pᵢ ℤ.* + n ℤ.* gcd[i,n] ℤ.+ j ℤ.* (↧ pᵢ ℤ.* gcd[i,n])
      ≡⟨ cong (ℤ._+_ (↥ pᵢ ℤ.* + n ℤ.* gcd[i,n]))
       $ cong (j ℤ.*_)
       $ ↧-/ i n
       ⟩
    ↥ pᵢ ℤ.* + n ℤ.* gcd[i,n] ℤ.+ j ℤ.* + n
      ≡⟨ cong (ℤ._+ j ℤ.* + n)
       $ cong (ℤ._* gcd[i,n])
       $ ℤ.*-comm (↥ pᵢ) (+ n)
       ⟩
    + n ℤ.* ↥ pᵢ ℤ.* gcd[i,n] ℤ.+ j ℤ.* + n
      ≡⟨ cong (ℤ._+ j ℤ.* + n)
       $ ℤ.*-assoc (+ n) (↥ pᵢ) gcd[i,n]
       ⟩
    + n ℤ.* (↥ pᵢ ℤ.* gcd[i,n]) ℤ.+ j ℤ.* + n
      ≡⟨ cong (ℤ._+ j ℤ.* + n)
       $ cong (+ n ℤ.*_)
       $ ↥-/ i n
       ⟩
    + n ℤ.* i ℤ.+ j ℤ.* + n
      ≡⟨ cong (ℤ._+ j ℤ.* + n)
       $ ℤ.*-comm (+ n) i
       ⟩
    i ℤ.* + n ℤ.+ j ℤ.* + n
      ≡⟨ ℤ.*-distribʳ-+ (+ n) i j ⟨
    (i ℤ.+ j) ℤ.* + n
      ∎

  ↧≡ : ↧ₙ pᵢ ℕ.* ↧ₙ qⱼ ℕ.* gcd[j,n]ₙ ℕ.* gcd[i,n]ₙ ≡ n ℕ.* n
  ↧≡ = begin
    ↧ₙ pᵢ ℕ.* ↧ₙ qⱼ ℕ.* gcd[j,n]ₙ ℕ.* gcd[i,n]ₙ
      ≡⟨ cong (ℕ._* gcd[i,n]ₙ)
       $ ℕ.*-assoc (↧ₙ pᵢ) (↧ₙ qⱼ) gcd[j,n]ₙ
       ⟩
    ↧ₙ pᵢ ℕ.* (↧ₙ qⱼ ℕ.* gcd[j,n]ₙ) ℕ.* gcd[i,n]ₙ
      ≡⟨ cong (ℕ._* gcd[i,n]ₙ)
       $ cong (↧ₙ pᵢ ℕ.*_)
       $ ℤ.abs-* (↧ qⱼ) (gcd j (+ n))
       ⟨
    ↧ₙ pᵢ ℕ.* ℤ.∣ (+ ↧ₙ qⱼ) ℤ.* gcd j (+ n) ∣ ℕ.* gcd[i,n]ₙ
      ≡⟨ cong (ℕ._* gcd[i,n]ₙ)
       $ cong (↧ₙ pᵢ ℕ.*_)
       $ cong ℤ.∣_∣
       $ ↧-/ j n
       ⟩
    ↧ₙ pᵢ ℕ.* n ℕ.* gcd[i,n]ₙ
      ≡⟨ cong (ℕ._* gcd[i,n]ₙ)
       $ ℕ.*-comm (↧ₙ pᵢ) n
       ⟩
    n ℕ.* ↧ₙ pᵢ ℕ.* gcd[i,n]ₙ
      ≡⟨ ℕ.*-assoc n (↧ₙ pᵢ) gcd[i,n]ₙ ⟩
    n ℕ.* (↧ₙ pᵢ ℕ.* gcd[i,n]ₙ)
      ≡⟨ cong (n ℕ.*_)
       $ ℤ.abs-* (↧ pᵢ) (gcd i (+ n))
       ⟨
    n ℕ.* ℤ.∣ + ↧ₙ pᵢ ℤ.* gcd i (+ n) ∣
      ≡⟨ cong (n ℕ.*_)
       $ cong ℤ.∣_∣
       $ ↧-/ i n
       ⟩
    n ℕ.* n
      ∎

-------------------------------------------------------------------------------
-- Definitions
-------------------------------------------------------------------------------

quot : (p q : ℚ) .{{ _ : NonZero q }} → ℚ
quot p q@record{} = (↥ p ℤ.* ↧ q) ℤ./ (↧ p ℤ.* ↥ q) / 1
  where instance _ = ℤ.i*j≢0 (↧ p) (↥ q)

rem : (p q : ℚ) .{{ _ : NonZero q }} → ℚ
rem p q@record{} = + ((↥ p ℤ.* ↧ q) ℤ.% (↧ p ℤ.* ↥ q)) / (↧ₙ p ℕ.* ↧ₙ q)
  where instance _ = ℤ.i*j≢0 (↧ p) (↥ q)

-------------------------------------------------------------------------------
-- Proofs
-------------------------------------------------------------------------------

↧ₙquot≡1 : ∀ (p q : ℚ) .{{_ : NonZero q }} → ↧ₙ (quot p q) ≡ 1
↧ₙquot≡1 p q@record{} = ↧ₙ[i/1]≡1 ((↥ p ℤ.* ↧ q) ℤ./ (↧ p ℤ.* ↥ q))
  where instance _ = ℤ.i*j≢0 (↧ p) (↥ q)

theorem₀ : ∀ (p q : ℚ) .{{_ : NonZero q }} → NonNegative (rem p q)
theorem₀ p q@record{} = mkℚ+-nonNeg (m ℕ./ ℕ.gcd m n) (n ℕ./ ℕ.gcd m n)
  where
  m = (↥ p ℤ.* ↧ q) ℤ.% (↧ p ℤ.* ↥ q)
  n = ↧ₙ p ℕ.* ↧ₙ q

  instance
    ↧p*↥q≢0 = ℤ.i*j≢0 (↧ p) (↥ q)
    g≢0     = ℕ.≢-nonZero (ℕ.gcd[m,n]≢0 m n (inj₂ (ℕ.≢-nonZero⁻¹ n)))
    n/g≢0   = ℕ.≢-nonZero (ℕ.n/gcd[m,n]≢0 m n {{gcd≢0 = g≢0}})

theorem₁ : ∀ (p q : ℚ) .{{_ : NonZero q }} → rem p q < ℚ.∣ q ∣
theorem₁ p@record{} q@record{}
  = *<* $ ℤ.*-cancelʳ-<-nonNeg gcd[m,n]
  $ begin-strict
    ↥ rem p q ℤ.* d ℤ.* gcd[m,n]     ≡⟨ cong (ℤ._* gcd[m,n])
                                      $ ℤ.*-comm d $ ↥ rem p q
                                      ⟨
    d ℤ.* ↥ rem p q ℤ.* gcd[m,n]     ≡⟨ ℤ.*-assoc d (↥ rem p q) gcd[m,n] ⟩
    d ℤ.* (↥ rem p q ℤ.* gcd[m,n])   ≡⟨ cong (d ℤ.*_) $ ↥-/ (+ m) n ⟩
    d ℤ.* + m                        <⟨ ℤ.*-monoˡ-<-pos d $ ℤ.+<+
                                      $ n%d<d (a ℤ.* d) (b ℤ.* c)
                                      ⟩
    d ℤ.* + ∣ b ℤ.* c ∣              ≡⟨ cong (d ℤ.*_) $ cong (+_) $ ℤ.abs-* b c ⟩
    d ℤ.* + (∣ b ∣ ℕ.* ∣ c ∣)        ≡⟨ cong (d ℤ.*_) $ ℤ.pos-* (↧ₙ p) ∣ c ∣ ⟩
    d ℤ.* (b ℤ.* ∣c∣)                ≡⟨ ℤ.*-assoc d b ∣c∣ ⟨
    d ℤ.* b ℤ.* ∣c∣                  ≡⟨ cong (ℤ._* ∣c∣) $ ℤ.*-comm d b ⟩
    b ℤ.* d ℤ.* ∣c∣                  ≡⟨ ℤ.*-comm (b ℤ.* d) ∣c∣ ⟩
    ∣c∣ ℤ.* (b ℤ.* d)                ≡⟨ cong (∣c∣ ℤ.*_) $ ↧-/ (+ m) n ⟨
    ∣c∣ ℤ.* (↧ rem p q ℤ.* gcd[m,n]) ≡⟨ ℤ.*-assoc ∣c∣ (↧ rem p q) gcd[m,n] ⟨
    ∣c∣ ℤ.* ↧ rem p q ℤ.* gcd[m,n]   ∎
  where
  open ℤ.≤-Reasoning

  a = ↥ p
  b = ↧ p
  c = ↥ q
  d = ↧ q
  ∣c∣ = + ∣ c ∣

  m = (a ℤ.* d) ℤ.% (b ℤ.* c)
  n = ∣ b ∣ ℕ.* ∣ d ∣

  instance
    b*c≢0 = ℤ.i*j≢0 b c
    g≢0   = ℕ.≢-nonZero (ℕ.gcd[m,n]≢0 m n (inj₂ (ℕ.≢-nonZero⁻¹ n)))
    n/g≢0 = ℕ.≢-nonZero (ℕ.n/gcd[m,n]≢0 m n {{gcd≢0 = g≢0}})

  gcd[m,n] = ℤ.gcd (+ m) (+ n)

theorem₂ : (p q : ℚ) .{{_ : NonZero q }} → quot p q * q + rem p q ≡ p
theorem₂ p@record{} q@record{} {{ nz }}
  = let open ≡-Reasoning in begin
      quot p q * q + rem p q

        ≡⟨ cong (_+ rem p q)
         $ *-def (quot p q) q
         ⟩

      (↥ quot p q ℤ.* c) / (↧ₙ (quot p q) ℕ.* dₙ) + rem p q

        ≡⟨ cong (_+ rem p q)
         $ /-cong { ↥ quot p q ℤ.* ↥ q } refl
         $ cong (ℕ._* ↧ₙ q)
         $ ↧ₙquot≡1 p q
         ⟩

      (↥ quot p q ℤ.* c) / (1 ℕ.* dₙ) + rem p q

        ≡⟨ cong (_+ rem p q)
         $ /-cong { ↥ quot p q ℤ.* ↥ q } refl
         $  ℕ.*-identityˡ (↧ₙ q)
         ⟩

      (↥ quot p q ℤ.* c) / dₙ + rem p q

        ≡⟨ cong (_+ rem p q)
         $ /-cong (cong (ℤ._* c) $ ↥[i/1]≡i $ n ℤ./ m) refl
         ⟩

      (n ℤ./ m ℤ.* c) / dₙ + rem p q

        ≡⟨ cong (_+ rem p q)
         $ *-cancelˡ-/ bₙ {(n ℤ./ m) ℤ.* c}
         ⟨

      (b ℤ.* (n ℤ./ m ℤ.* c)) / (bₙ ℕ.* dₙ) + + (n ℤ.% m) / (bₙ ℕ.* dₙ)

        ≡⟨ i/n+j/n≡[i+j]/n
             (b ℤ.* (n ℤ./ m ℤ.* c))
             (+ (n ℤ.% m))
             (bₙ ℕ.* dₙ)
         ⟩

      (b ℤ.* (n ℤ./ m ℤ.* c) ℤ.+ + (n ℤ.% m)) / (bₙ ℕ.* dₙ)

        ≡⟨ /-cong
             ( cong (ℤ._+ + (n ℤ.% m))
             $ cong (b ℤ.*_)
             $ ℤ.*-comm (n ℤ./ m) c
             ) refl
         ⟩

      (b ℤ.* (c ℤ.* (n ℤ./ m)) ℤ.+ + (n ℤ.% m)) / (bₙ ℕ.* dₙ)

        ≡⟨ /-cong
             ( cong (ℤ._+ + (n ℤ.% m))
             $ ℤ.*-assoc b c (n ℤ./ m)
             ) refl
         ⟨

      (m ℤ.* (n ℤ./ m) ℤ.+ + (n ℤ.% m)) / (bₙ ℕ.* dₙ)

        ≡⟨ /-cong
             ( cong (ℤ._+ + (n ℤ.% m))
             $ ℤ.*-comm m (n ℤ./ m)
             ) refl
         ⟩

      (n ℤ./ m ℤ.* m ℤ.+ + (n ℤ.% m)) / (bₙ ℕ.* dₙ)

        ≡⟨ /-cong (ℤ.+-comm (n ℤ./ m ℤ.* m) (+ (n ℤ.% m))) refl ⟩

      (+ (n ℤ.% m) ℤ.+ (n ℤ./ m ℤ.* m)) / (bₙ ℕ.* dₙ)

        ≡⟨ /-cong lemma₁ refl ⟩

      (+ (n ℤ.%ℕ ∣ m ∣) ℤ.+ (n ℤ./ℕ ∣ m ∣) ℤ.* + ∣ m ∣) / (bₙ ℕ.* dₙ)

        ≡⟨ /-cong (a≡a%ℕn+[a/ℕn]*n n ∣ m ∣) refl ⟨

      n / (bₙ ℕ.* dₙ)

        ≡⟨ *-cancelʳ-/ dₙ {a} {bₙ} ⟩

      a / bₙ

        ≡⟨ ↥p/↧p≡p p ⟩

      p

    ∎
 where
  a = ↥ p
  b = ↧ p
  c = ↥ q
  d = ↧ q
  bₙ = ↧ₙ p
  dₙ = ↧ₙ q
  n = a ℤ.* d
  m = b ℤ.* c

  instance
    _ = ℤ.i*j≢0 (↧ p) (↥ q)

  *-def : ∀ p q → p * q ≡ (↥ p ℤ.* ↥ q) / (↧ₙ p ℕ.* ↧ₙ q)
  *-def record{} record{} = refl

  lemma₀ : ∀ n m .{{ _ : ℕ.NonZero ∣ m ∣ }} →
          n ℤ./ m ℤ.* m ≡ n ℤ./ℕ ∣ m ∣ ℤ.* + ∣ m ∣
  lemma₀ n (-[1+ m ])
    = let open ≡-Reasoning in begin

       ℤ.-1ℤ ℤ.* (n ℤ./ℕ suc m) ℤ.* -[1+ m ]

    ≡⟨ ℤ.*-assoc ℤ.-1ℤ (n ℤ./ℕ suc m) -[1+ m ] ⟩

       ℤ.-1ℤ ℤ.* ((n ℤ./ℕ suc m) ℤ.* -[1+ m ])

    ≡⟨ cong (ℤ.-1ℤ ℤ.*_) $ ℤ.*-comm (n ℤ./ℕ suc m) (-[1+ m ]) ⟩

       ℤ.-1ℤ ℤ.* (-[1+ m ] ℤ.* (n ℤ./ℕ suc m))

    ≡⟨ ℤ.*-assoc ℤ.-1ℤ (-[1+ m ]) (n ℤ./ℕ suc m) ⟨

       + (1 ℕ.* suc m) ℤ.* (n ℤ./ℕ suc m)

    ≡⟨ ( cong (ℤ._* (n ℤ./ℕ suc m)) $ cong (+_) $ ℕ.*-identityˡ (suc m)) ⟩

       + (suc m) ℤ.* (n ℤ./ℕ suc m)

    ≡⟨ ℤ.*-comm (+ (suc m)) (n ℤ./ℕ suc m) ⟩

       n ℤ./ℕ (suc m) ℤ.* + (suc m)

    ∎
  lemma₀ (-[1+ n ]) (+ m) with suc n ℕ.% m
  ... | ℕ.zero
    = cong (ℤ._* + m)
    $ ℤ.*-identityˡ (ℤ.- (+ (suc n ℕ./ m)))
  ... | suc _
    = cong (Sign.- ℤ.◃_)
    $ cong (m ℕ.+_)
    $ cong (ℕ._* m)
    $ ℕ.+-identityʳ (suc n ℕ./ m)
  lemma₀ (+ n) (+ m)
    = cong (ℤ._* + m)
    $ ℤ.*-identityˡ (+ (n ℕ./ m))

  lemma₁ : + (n ℤ.% m) ℤ.+ (n ℤ./ m ℤ.* m) ≡
           + (n ℤ.%ℕ ∣ m ∣) ℤ.+ (n ℤ./ℕ ∣ m ∣ ℤ.* + ∣ m ∣)
  lemma₁ rewrite lemma₀ n m {{_}} with n
  ... | + _      = refl
  ... | -[1+ _ ] = refl

-------------------------------------------------------------------------------
