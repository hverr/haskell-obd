module System.Hardware.ELM327.Utils.Monad where

-- | Convert 'Maybe a' to 'Either a b'.
maybeToLeft :: b -> Maybe a -> Either a b
maybeToLeft _ (Just x) = Left x
maybeToLeft x Nothing  = Right x

-- | Convert 'Maybe a' to 'Either b a'.
maybeToRight :: b -> Maybe a -> Either b a
maybeToRight _ (Just x) = Right x
maybeToRight x Nothing = Left x
