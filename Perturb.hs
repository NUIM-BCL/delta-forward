{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TupleSections #-}

module Perturb (perturbs, grad, grad_)
where

import Prelude.Unicode
import Data.Maybe (fromJust)

-- A perturber Pert a applies the given operation to some leaf Double in a.
type Pert a = (Double → Double) → a → a

-- 'perturbs' takes an object and returns a list of perturbers of that
-- object.  A perturber takes a function (Double → Double) that, given
-- the value of a leaf, gives the new value for that leaf, and an
-- aggregate object, and gives the result of applying that function to
-- change some particular leaf.
class Perturbable a where
  perturbs ∷ a → [Pert a]

instance Perturbable Double where
  perturbs _x = [id]

perturbsConstruct0 ∷ z → z → [Pert z]
perturbsConstruct0 _constructor _x = []

perturbsConstruct1 ∷ Perturbable a ⇒ (a → z) → (z → a) → z → [Pert z]
perturbsConstruct1 constructor accessor x =
  [\f z → constructor (p f (accessor z)) | p ← perturbs (accessor x)]

perturbsConstruct2 ∷ (Perturbable a, Perturbable b) ⇒ (a → b → z) → (z → a) → (z → b) → z → [Pert z]
perturbsConstruct2 constructor accessor1 accessor2 x =
  [\f z → flip constructor (accessor2 z) (p f (accessor1 z)) | p ← perturbs (accessor1 x)] ⧺
  [\f z →      constructor (accessor1 z) (p f (accessor2 z)) | p ← perturbs (accessor2 x)]

instance Perturbable a ⇒ Perturbable [a] where
  perturbs x@[] = perturbsConstruct0 [] x
  perturbs x@(_:_) = perturbsConstruct2 (:) head tail x

instance (Perturbable a, Perturbable b) ⇒ Perturbable (a,b) where
  perturbs x@(_,_) = perturbsConstruct2 (,) fst snd x

instance Perturbable a ⇒ Perturbable (Maybe a) where
  perturbs x@Nothing = perturbsConstruct0 Nothing x
  perturbs x@(Just _) =  perturbsConstruct1 Just fromJust x

instance (Perturbable a, Perturbable b) ⇒ Perturbable (Either a b) where
  perturbs x@(Left  _) = perturbsConstruct1 Left  (either id ⊥)  x
  perturbs x@(Right _) = perturbsConstruct1 Right (either ⊥  id) x

-- asymmetric difference, error is O(ε)
grad ∷ (Perturbable a) ⇒ Double → (a → Double) → a → a
grad ε f x = foldl step x $ perturbs x
  where
    y = f x
    step dx p = p (const ((f (p (+ε) x) - y) / ε)) dx

-- symmetric difference, error is O(ε²)
grad_ ∷ (Perturbable a) ⇒ Double → (a → Double) → a → a
grad_ ε f x = foldl step x $ perturbs x
  where
    step dx p = p (const ((f (p (+ε) x) - f (p (+(-ε)) x)) / (2⋅ε))) dx

-- Examples:

-- *Perturb> grad 1e-6 sin 0
-- 0.9999999999998334

-- *Perturb> grad 1e-6 (\(x,y) → x^2 + y^3) (1,2)
-- (2.0000010003684565,12.000006002210739)

-- *Perturb> grad 1e-6 (\(x,(y,z)) → x^2 + y^3 + z^5) (1,(2,3))
-- (2.0000009897103155,(12.000005995105312,405.00027006373784))

-- *Perturb> grad 1e-3 (sqrt ∘ sum ∘ map (^2)) [1..5]
-- [0.13490616545741574,0.2697424594266806,0.40457630202705985,0.5394076933935565,0.6742366336585093]

-- *Perturb> grad 1e-9 (\(x,[y,z]) → x + y⋅z) (1, [2, 3])
-- (1.000000082740371,[3.000000248221113,2.000000165480742])
