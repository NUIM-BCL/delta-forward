{-# LANGUAGE UnicodeSyntax #-}

module Perturb (perturbs)
where

import Prelude.Unicode

-- 'perturbs' takes an ε and an object and returns a list of
-- perturbers.  A perturber takes a function (Double→Double) that,
-- given the value of a leaf, gives the new value for that leaf, and
-- an aggregate object, and gives the result of applying that function
-- to change one leaf.
class Perturbable a where
  perturbs ∷ a → [(Double→Double)→a→a]

instance Perturbable Double where
  perturbs x = [id]

instance Perturbable a ⇒ Perturbable [a] where
  perturbs [] = []
  perturbs (x:xs) =
    [\g (y:ys) → x_ g y : ys | x_ ← perturbs x] ++
    [\g (y:ys) → y : xs_ g ys | xs_ ← perturbs xs]

instance (Perturbable a, Perturbable b) ⇒ Perturbable (a,b) where
  perturbs (x,y) =
    [ \g (x'',y'') → (x_ g x'',y'') | x_ ← perturbs x] ++
    [ \g (x'',y'') → (x'', y_ g y'') | y_ ← perturbs y]

instance Perturbable a ⇒ Perturbable (Maybe a) where
  perturbs Nothing = []
  perturbs (Just x) = [\g (Just x'') → Just (x_ g x'') | x_ ← perturbs x]

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
    step dx p = p (const ((f (p (+ε) x) - f (p (+(-ε)) x)) / (2*ε))) dx

-- Examples:

-- *Perturb> grad 1e-6 sin 0
-- 0.9999999999998334

-- *Perturb> grad 1e-6 (\(x,y) → x^2 + y^3) (1,2)
-- (2.0000010003684565,12.000006002210739)

-- *Perturb> grad 1e-6 (\(x,(y,z)) → x^2 + y^3 + z^5) (1,(2,3))
-- (2.0000009897103155,(12.000005995105312,405.00027006373784))

-- *Perturb> grad 1e-3 (sqrt ∘ sum ∘ map (^2)) [1..5]
-- [0.13490616545741574,0.2697424594266806,0.40457630202705985,0.5394076933935565,0.6742366336585093]
