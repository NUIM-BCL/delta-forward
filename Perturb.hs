{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}

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

-- instance Perturbable a ⇒ Perturbable [a] where
--   perturbs ε [] = []
--   perturbs ε (x:xs) = [] ++ [(x,XXX):ys |  ← perturbs ε xs]

-- map (:xs) (perturbs ε x) ++ map (x:) (perturbs ε xs)

instance (Perturbable a, Perturbable b) ⇒ Perturbable (a,b) where
  perturbs ε (x,y) =
    [ ((x',y),(\z (x'',y'')→(x_ z x'',y''))) | (x', x_) ← perturbs ε x] ++
    [ ((x,y'),(\z (x'',y'')→(x'', y_ z y''))) | (y', y_) ← perturbs ε y]

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
