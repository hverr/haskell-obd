-- | Collection of helper functions to deal with monads.
module System.Hardware.ELM327.Utils.Monad where

import Control.Monad.Except (MonadError, throwError)

-- | Convert 'Maybe a' to 'Either a b'.
maybeToLeft :: b -> Maybe a -> Either a b
maybeToLeft _ (Just x) = Left x
maybeToLeft x Nothing  = Right x

-- | Convert 'Maybe a' to 'Either b a'.
maybeToRight :: b -> Maybe a -> Either b a
maybeToRight _ (Just x) = Right x
maybeToRight x Nothing = Left x

-- | Convert 'Maybe a' to @throwError a@.
maybeToExcept :: MonadError e m => Maybe e -> a -> m a
maybeToExcept (Just x) _ = throwError x
maybeToExcept Nothing x = return x

-- | Throw an exception on 'Nothing'.
orThrow :: MonadError e m => Maybe a -> e -> m a
orThrow Nothing e = throwError e
orThrow (Just x) _ = return x
