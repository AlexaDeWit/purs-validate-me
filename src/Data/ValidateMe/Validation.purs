module Data.ValidateMe.Validation where

import Prelude
import Data.Function (applyFlipped)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (sequence)

data InputState a
  = Initial
  | Touched a

data ValidatedField e a
  = ValidatedField
    { inputState :: InputState a
    , initialValue :: a
    , validationRules :: Array (a -> Maybe e)
    }

value :: forall e a. ValidatedField e a -> a
value (ValidatedField r) = case r.inputState of
  Initial -> r.initialValue
  Touched a -> a

errors :: forall e a. ValidatedField e a -> Maybe (Array e)
errors (ValidatedField r) = case r.inputState of
  Initial -> Nothing
  Touched a -> sequence $ map (applyFlipped a) r.validationRules

data ValidationResult e a
  = Untouched a
  | WithErrors (Array e) a
  | Valid a

runValidation :: forall e a. ValidatedField e a -> ValidationResult e a
runValidation (ValidatedField f) = case f.inputState of
  Initial -> Untouched f.initialValue
  Touched a ->
    f.validationRules
      # map (applyFlipped a)
      >>> sequence
      >>> maybe (Valid a) (flip WithErrors a)

cata :: forall e a r. (a -> r) -> (Array e -> a -> r) -> (a -> r) -> ValidatedField e a -> r
cata fromInitial fromErrored fromValid =
  runValidation
    >>> case _ of
        Untouched a -> fromInitial a
        WithErrors errs a -> fromErrored errs a
        Valid a -> fromValid a

cata' :: forall e a r. (Array e -> a -> r) -> (a -> r) -> ValidatedField e a -> r
cata' fromErrored fromValid = cata fromValid fromErrored fromValid
