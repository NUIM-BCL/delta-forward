{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}

module Perturb (perturbs)
where

import Prelude.Unicode

-- 'perturb' takes an ε and an object and returns a list of perturbed primal objects
class Perturbable a where
  perturbs ∷ Double → a → [a]
  dividedDifference ∷ Double → a → a → a

instance Perturbable Double where
  perturbs ε x = [x + ε]
  dividedDifference ε x y = (y - x) / ε

instance Perturbable a ⇒ Perturbable [a] where
  perturbs ε [] = []
  perturbs ε (x:xs) = map (:xs) (perturbs ε x) ++ map (x:) (perturbs ε xs)
  dividedDifference ε xs ys = zipWith (dividedDifference ε) xs ys

instance (Perturbable a, Perturbable b) ⇒ Perturbable (a,b) where
  perturbs ε (x,y) = map (,y) (perturbs ε x) ++ map (x,) (perturbs ε y)
  dividedDifference ε (x,y) (x1,y1) = (dividedDifference ε x x1, dividedDifference ε y y1)

-- grad ∷ (Perturbable a) ⇒ Double → a → 
-- grad ε x =
