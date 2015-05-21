{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | CEF Log Format
--
-- This library implements ArcSight CEF Revision 20 released on 06/05/2013
--
--  See: <https://protect724.hp.com/servlet/JiveServlet/downloadBody/1072-102-6-4697/CommonEventFormat.pdf>

module System.Log.CEF
  ( CEFEvent (..)
  , log
  -- * Extensions
  , module System.Log.CEF.Extensions
  ) where

--------------------------------------------------------------------------------
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy      as BL
import           Data.Foldable
import           Data.List                 (intersperse)
import           Data.Monoid
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import           Prelude                   hiding (log)
--------------------------------------------------------------------------------
import           System.Log.CEF.Extensions
--------------------------------------------------------------------------------

-- Cef log line:
-- CEF:Version|Device Vendor|Device Product|Device|Version|Signature ID|Name|Severity|[Extension]

data CEFEvent
  = CEFEvent { deviceVendor  :: T.Text
             , deviceProduct :: T.Text
             , deviceVersion :: T.Text
             , signatureId   :: T.Text -- ^ Is a unique identifier per event-type.
             , name          :: T.Text -- ^ Is a string representing a human-readable and understandable description of the event.
             , severity      :: Int    -- ^ Reflects the importance of the event. Must be in range @[0..10]@.
             , extensions    :: Extensions
             }


-- |
-- >>> :set -XOverloadedStrings
-- >>> let exampleEvent = CEFEvent "Acme Corp" "Acmetorazor" "2.1" "cool" "MyNameIsCool" 10 (applicationProtocol "PUT")
-- >>> toLazyByteString $ log exampleEvent
-- "CEF:0|Acme Corp|Acmetorazor|2.1|MyNameIsCool|10|app=PUT\n"
log :: CEFEvent -> Builder
log CEFEvent{..} = mconcat (intersperse sep fields) <> "\n"
  where sep = "|"
        fields = [ "CEF:0"
                 , renderHeader deviceVendor
                 , renderHeader deviceProduct
                 , renderHeader deviceVersion
                 , renderHeader name
                 , intDec severity
                 , extensionsBuilder extensions
                 ]

-- |
-- >>> toLazyByteString (renderHeader "")
-- ""
-- >>> toLazyByteString (renderHeader "foo|bar")
-- "foo\\|bar"
-- >>> toLazyByteString (renderHeader "foo\\bar")
-- "foo\\\\bar"
renderHeader :: T.Text -> Builder
renderHeader cs = T.encodeUtf8Builder $ T.concatMap escape cs
  where escape '|'  = "\\|"
        escape '\\' = "\\\\"
        escape i = T.singleton i



