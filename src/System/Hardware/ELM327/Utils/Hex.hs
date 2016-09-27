-- | Collection of functions to deal with hexadecimal encoding.
module System.Hardware.ELM327.Utils.Hex where

import Data.List.Split (chunksOf)
import Data.Word (Word8)
import Numeric (readHex)

-- | Convert an even list of hexadecimal characters to a list of Word8 values.
--
-- If the list is not even, or the string contains a non-hexadecimal
-- character, 'Nothing' is returned.
hexToBytes :: String -> Maybe [Word8]
hexToBytes = mapM pairToWord8 . chunksOf 2
  where
    pairToWord8 s@[_, _] | [(x, "")] <- readHex s = Just x
                         | otherwise = Nothing
    pairToWord8 _ = Nothing
