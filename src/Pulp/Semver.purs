module Pulp.Semver where

import Prelude

import Data.Maybe (Maybe(), fromMaybe)
import Data.Monoid (Monoid)
import Data.Foldable (Foldable, foldr)

data Version = Version Int Int Int (Maybe String)

instance eqVersion :: Eq Version where
  eq (Version a1 a2 a3 a4) (Version b1 b2 b3 b4) =
    eq a1 b1 && eq a2 b2 && eq a3 b3 && eq a4 b4

instance ordVersion :: Ord Version where
  compare (Version a1 a2 a3 a4) (Version b1 b2 b3 b4) =
    if a1 /= b1 then compare a1 b1
    else if a2 /= b2 then compare a2 b2
         else if a3 /= b3 then compare a3 b3
              else compare a4 b4

instance showVersion :: Show Version where
  show (Version major minor patch qualifier) =
    show major ++ "." ++ show minor ++ "." ++ show patch
      ++ fromMaybe "" qualifier

data VersionRange = Lt Version
                  | LtEq Version
                  | Gt Version
                  | GtEq Version
                  | Eq Version
                  | Or VersionRange VersionRange
                  | And VersionRange VersionRange
                  | Any
                  | None

minV :: Version -> Version -> Version
minV a b = if a > b then b else a

maxV :: Version -> Version -> Version
maxV a b = if a < b then b else a

-- min :: VersionRange -> VersionRange
-- min Any = Any
-- min None = None
-- min (Eq v) = GtEq v
-- min (Lt _) = Any
-- min (LtEq _) = Any
-- min v@(Gt _) = v
-- min v@(GtEq _) = v

-- min (Or a b) =
--   let a' = min a
--       b' = min b
--       min' None a = a
--       min' a None = a
--       min' Any a = Any
--       min' a Any = Any
--       min' (Gt a) (Gt b) = Gt $ minV a b
--       min' (GtEq a) (GtEq b) = GtEq $ minV a b
--       min' (Gt a) (GtEq b) | a < b = Gt a
--       min' (Gt a) (GtEq b) | a >= b = GtEq b
--       min' (GtEq b) (Gt a) | a < b = Gt a
--       min' (GtEq b) (Gt a) | a >= b = GtEq b
--   in min' (min a) (min b)

-- min (And a b) =
--   let a' = min a
--       b' = min b
--       min' None a = None
--       min' a None = None
--       min' Any a = a
--       min' a Any = a
--       min' (Gt a) (Gt b) = Gt $ maxV a b
--       min' (GtEq a) (GtEq b) = GtEq $ maxV a b
--       min' (Gt a) (GtEq b) | a > b = Gt a
--       min' (Gt a) (GtEq b) | a <= b = GtEq b
--       min' (GtEq b) (Gt a) | a > b = Gt a
--       min' (GtEq b) (Gt a) | a <= b = GtEq b
--   in min' (min a) (min b)

-- max :: VersionRange -> VersionRange
-- max Any = Any
-- max None = None
-- max (Eq v) = GtEq v
-- max (Gt _) = Any
-- max (GtEq _) = Any
-- max v@(Lt _) = v
-- max v@(LtEq _) = v

-- max (Or a b) =
--   let a' = max a
--       b' = max b
--       max' None a = a
--       max' a None = a
--       max' Any a = Any
--       max' a Any = Any
--       max' (Lt a) (Lt b) = Lt $ maxV a b
--       max' (LtEq a) (LtEq b) = LtEq $ maxV a b
--       max' (Lt a) (LtEq b) | a < b = Lt a
--       max' (Lt a) (LtEq b) | a >= b = LtEq b
--       max' (LtEq b) (Lt a) | a < b = Lt a
--       max' (LtEq b) (Lt a) | a >= b = LtEq b
--   in max' (max a) (max b)

-- max (And a b) =
--   let a' = max a
--       b' = max b
--       max' None a = None
--       max' a None = None
--       max' Any a = a
--       max' a Any = a
--       max' (Lt a) (Lt b) = Lt $ minV a b
--       max' (LtEq a) (LtEq b) = LtEq $ minV a b
--       max' (Lt a) (LtEq b) | a > b = Lt a
--       max' (Lt a) (LtEq b) | a <= b = LtEq b
--       max' (LtEq b) (Lt a) | a > b = Lt a
--       max' (LtEq b) (Lt a) | a <= b = LtEq b
--   in max' (max a) (max b)

-- collapse :: VersionRange -> VersionRange
-- collapse v@(Or _ _) =
--   let collapse' Any Any = Any
--       collapse' _ _ = v
--   in collapse' (min v) (max v)
-- collapse v@(And _ _) =
--   let collapse'
--       collapse' _ _ = v
--   in collapse' (min v) (max v)
-- collapse v = v

union :: VersionRange -> VersionRange -> VersionRange
union Any a = Any
union a Any = Any
union None a = a
union a None = a
union a@(Eq _) b@(Eq _) = Or a b
union b a@(Eq _) = union b a
union a@(Lt _) b@(Gt _) = union b a
union a@(LtEq _) b@(Gt _) = union b a
union a@(Lt _) b@(GtEq _) = union b a
union a@(LtEq _) b@(GtEq _) = union b a
union a b = Or a b

intersection :: VersionRange -> VersionRange -> VersionRange
intersection Any a = a
intersection a Any = a
intersection None a = None
intersection a None = None
intersection (Eq a) (Eq b) = if a == b then Eq a else None
intersection (Eq a) b = if contains b a then Eq a else None
intersection a b@(Eq _) = intersection b a
intersection a@(Lt _) b@(Gt _) = intersection b a
intersection a@(LtEq _) b@(Gt _) = intersection b a
intersection a@(Lt _) b@(GtEq _) = intersection b a
intersection a@(LtEq _) b@(GtEq _) = intersection b a
intersection a b = And a b

intersections :: forall f. (Foldable f) => f VersionRange -> VersionRange
intersections = foldr intersection Any

instance semigroupVersionRange :: Semigroup VersionRange where
  append = union

instance monoidVersionRange :: Monoid VersionRange where
  mempty = None

instance showVersionRange :: Show VersionRange where
  show (Lt v) = "<" ++ show v
  show (LtEq v) = "<=" ++ show v
  show (Gt v) = ">" ++ show v
  show (GtEq v) = ">=" ++ show v
  show (Eq v) = show v
  show (Or a b) = show a ++ " || " ++ show b
  show (And a b) = show a ++ " " ++ show b
  show Any = "*"
  show None = "None"

contains :: VersionRange -> Version -> Boolean
contains Any _ = true
contains None _ = false
contains (Eq a) b = a == b
contains (Lt a) b = b < a
contains (Gt a) b = b > a
contains (LtEq a) b = b <= a
contains (GtEq a) b = b >= a
contains (Or a b) c = (contains a c) || (contains b c)
contains (And a b) c = (contains a c) && (contains b c)
