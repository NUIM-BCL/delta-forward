{-# LANGUAGE UnicodeSyntax #-}

module Perturb (perturbs)
where

import Prelude.Unicode

-- 'perturbs' takes an ε and an object and returns a list of perturbed
-- primal objects paired with injectors that swap in a new value for
-- the perturbed one.
class Perturbable a where
  perturbs ∷ Double → a → [(a,Double→a→a)]

instance Perturbable Double where
  perturbs ε x = [(x+ε,const)]

instance Perturbable a ⇒ Perturbable [a] where
  perturbs ε [] = []
  perturbs ε (x:xs) =
    [(x':xs, \z (y:ys) → x_ z y : ys) | (x',x_) ← perturbs ε x] ++
    [(x:xs',\z (y:ys) → y : xs_ z ys) | (xs',xs_) ← perturbs ε xs]

instance (Perturbable a, Perturbable b) ⇒ Perturbable (a,b) where
  perturbs ε (x,y) =
    [ ((x',y),(\z (x'',y'')→(x_ z x'',y''))) | (x', x_) ← perturbs ε x] ++
    [ ((x,y'),(\z (x'',y'')→(x'', y_ z y''))) | (y', y_) ← perturbs ε y]

instance Perturbable a ⇒ Perturbable (Maybe a) where
  perturbs ε Nothing = []
  perturbs ε (Just x) = [(Just x', \z (Just x'') → Just (x_ z x'')) | (x',x_) ← perturbs ε x]

grad ∷ (Perturbable a) ⇒ Double → (a → Double) → a → a
grad ε f x = foldl step x $ perturbs ε x
  where
    y = f x
    step dx (x',x_) = x_ ((f x' - y)/ε) dx

-- Examples:

-- *Perturb> grad 1e-6 sin 0
-- 0.9999999999998334

-- *Perturb> grad 1e-6 (\(x,y) -> x^2 + y^3) (1,2)
-- (2.0000010003684565,12.000006002210739)

-- *Perturb> grad 1e-6 (\(x,(y,z)) -> x^2 + y^3 + z^5) (1,(2,3))
-- (2.0000009897103155,(12.000005995105312,405.00027006373784))

-- *Perturb> grad 1e-3 (sqrt.sum.map (^2)) [1..5]
-- [0.13490616545741574,0.2697424594266806,0.40457630202705985,0.5394076933935565,0.6742366336585093]
