-- | A collection of error types.
module System.Hardware.ELM327.Errors where

import Control.Lens (Prism', prism')

-- | Possible errors when issuing OBD commands.
data OBDError = OBDErrorMessage OBDErrorMessage
              | OBDDecodeError OBDDecodeError
              deriving (Eq, Show)

-- | Error messages sent by the ELM327 when issuing OBD commands
data OBDErrorMessage = UnableToConnect
                     | BusInitError
                     | NoData
                     deriving (Eq, Show)

-- | A prism between 'String' and 'ELM327Error'.
obdErrorMessage :: Prism' String OBDErrorMessage
obdErrorMessage = prism' conv mConv
  where
    conv UnableToConnect = "UNABLE TO CONNECT"
    conv BusInitError = "BUS INIT: ... ERROR"
    conv NoData = "NO DATA"

    mConv "UNABLE TO CONNECT" = Just UnableToConnect
    mConv "BUS INIT: ... ERROR" = Just BusInitError
    mConv "NO DATA" = Just NoData
    mConv _ = Nothing

-- | Possible decode errors when issuing OBD commands.
data OBDDecodeError = NotEnoughBytesError
                    | HexDecodeError
                    | NoResponseHeaderError
                    | EmptyResponseError
                    deriving (Eq, Show)
