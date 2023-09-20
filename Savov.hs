{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant $" #-}

import Control.Arrow (Arrow(..))
import Data.Map (Map)
import Data.List (partition)

type ProbabilityOf a = (Double, a)
type Distribution a = [ProbabilityOf a]

newtype RandomVariable a =
    RandomVariable
        -- invariant: sum (fst <$> tva) == 1
        { getRandomVariable :: Distribution a
        }
    deriving (Show)

simplify :: forall a. Eq a => RandomVariable a -> RandomVariable a
simplify = RandomVariable . go . getRandomVariable
  where
    go :: [(Double, a)] -> [(Double, a)]
    go [] = []
    go ((p, b) : xs) = (p + sum (fst <$> rip), b) : go rest
      where
        (rip, rest) = partition ((== b) . snd) xs

instance Functor RandomVariable where
    fmap :: (a -> b) -> RandomVariable a -> RandomVariable b
    fmap f = RandomVariable . fmap (second f) . getRandomVariable

instance Applicative RandomVariable where
  pure :: a -> RandomVariable a
  pure a = RandomVariable [(1, a)] 

  (<*>) :: RandomVariable (a -> b) -> RandomVariable a -> RandomVariable b
  (RandomVariable dfab) <*> (RandomVariable dva) =
    RandomVariable $ [(p1 * p2, f a) | (p1, f) <- dfab, (p2, a) <- dva]

instance Monad RandomVariable where
  (>>=) :: RandomVariable a -> (a -> RandomVariable b) -> RandomVariable b
  (RandomVariable da) >>= farvb = RandomVariable $ da >>= (\(pa, a) -> fmap (first (* pa)) $ getRandomVariable $ farvb a)

------

data Den = Ponedelnik | Vtornik | Srqda
    deriving (Eq, Show)
data Vali = Vali | NeVali
    deriving (Eq, Show)

daliValiBg :: Den -> RandomVariable Vali
daliValiBg Ponedelnik = RandomVariable $ [(0.1, Vali), (0.9, NeVali)]
daliValiBg Vtornik    = RandomVariable $ [(0.7, Vali), (0.3, NeVali)]
daliValiBg Srqda      = RandomVariable $ [(0.0, Vali), (1.0, NeVali)]

timeMachine :: RandomVariable Den
timeMachine = RandomVariable $ [(0.6, Ponedelnik), (0.25, Vtornik), (0.15, Srqda)]

dneskaNzKogaShteValiLiBreVuprositelna :: RandomVariable Vali
dneskaNzKogaShteValiLiBreVuprositelna = timeMachine >>= daliValiBg

-- >>> simplify $ dneskaNzKogaShteValiLiBreVuprositelna
-- RandomVariable {getRandomVariable = [(0.235,Vali),(0.765,NeVali)]}

