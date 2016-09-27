module System.Hardware.ELM327.Errors where

import Control.Lens (Prism', prism')

-- | Error messages sent by the ELM327.
data Error = UnableToConnect
           | NoData
           deriving (Eq, Show)

-- | A prism between 'String' and 'ELM327Error'.
errorMessage :: Prism' String Error
errorMessage = prism' conv mConv
  where
    conv UnableToConnect = "UNABLE TO CONNECT"
    conv NoData = "NO DATA"

    mConv "UNABLE TO CONNECT" = Just UnableToConnect
    mConv "NO DATA" = Just NoData
    mConv _ = Nothing
