module Data.Validation where

import Data.Bifunctor (lmap)
import Data.Either (Either(..), either, hush)
import Data.Functor ((<#>), class Functor)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Semigroup (append, class Semigroup)
import Data.Tuple (Tuple(..))
import Prelude (pure, const, (<<<), map, ($))

newtype Validator e a r
  = Validator (a -> Either e r)

derive instance newtypeValidator :: Newtype (Validator e a r) _

instance functorValidator :: Functor (Validator e a) where
  map f g = wrap $ map (map f) (unwrap g)

instance semigroupValidator :: Semigroup (Validator (Array e) a r) where
  append (Validator f) (Validator g) = Validator h
    where
    h :: a -> Either (Array e) r
    h a = case f a of
      Left errs -> Left $ either (append errs) (const errs) (g a)
      Right rs -> g a

validator' :: forall a e. (a -> Boolean) -> e -> Validator e a a
validator' p e = validator f
  where
  f :: a -> Either e a
  f i = case p i of
    true -> Right i
    false -> Left e

validator :: forall a e r. (a -> Either e r) -> Validator e a r
validator = Validator

validator_ :: forall a e r. (Newtype r a) => (a -> Boolean) -> e -> Validator e a r
validator_ p e = validator' p e <#> wrap

touch :: forall a e r. ValidationG e a r -> ValidationG e a r
touch (ValidationG _ v a) = ValidationG Dirty v a

data InputState
  = Initial
  | Dirty

data ValidationG e a r
  = ValidationG InputState (Validator e a r) a

type Validation a e r
  = ValidationG a (Array e) r

validation :: forall e a r. Validator e a r -> a -> ValidationG e a r
validation = ValidationG Initial

inputState :: forall e a r. ValidationG e a r -> InputState
inputState (ValidationG i _ _) = i

inputValue :: forall e a r. ValidationG e a r -> a
inputValue (ValidationG _ _ a) = a

validate :: forall e a r. ValidationG e a r -> Either e r
validate (ValidationG _ (Validator p) a) = p a

validate_ :: forall e a r. ValidationG e a r -> Maybe r
validate_ (ValidationG _ (Validator p) a) = hush $ p a

updateValidation :: forall e a r. ValidationG e a r -> a -> ValidationG e a r
updateValidation (ValidationG _ v _) = ValidationG Dirty v

combine :: forall e a1 r1 a2 r2. Validation a1 r1 -> Validation a2 r2 -> Validation (Tuple a1 a2) (Tuple r1 r2)
combine (ValidationG _ v1 i1) (ValidationG _ v2 i2) = ValidationG Dirty (Validator combined) (Tuple i1 i2)
  where
  combined :: Tuple a1 a2 -> Either e (Tuple r1 r2)
  combined (Tuple f s) = result
    where
    result :: Either (Array e) (Tuple r1 r2)
    result = case l of
      Left errs -> Left $ either (append errs) (const errs) r
      Right rs -> map (Tuple rs) r

    l :: Either (Array e) r1
    l = unwrap v1 $ f

    r :: Either (Array e) r2
    r = unwrap v2 $ s
