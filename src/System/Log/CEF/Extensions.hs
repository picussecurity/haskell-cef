{-# LANGUAGE OverloadedStrings #-}

module System.Log.CEF.Extensions
  ( Extensions
  , emptyExtensions
  , extensionsBuilder
  , customExtension
  -- ** Predefined Extensions
  -- | See /Chapter 2: ArcSight Extension Dictionary/ from the reference
  , IPv6Address
  , IPv4Address
  , MACAddress
  , TimeStamp
  , deviceAction
  , deviceCustomIPv6Address1
  , deviceCustomIPv6Address2
  , deviceCustomIPv6Address3
  , deviceCustomIPv6Address4
  , applicationProtocol
  , deviceEventCategory
  , deviceCustomFloatingPoint1
  , deviceCustomFloatingPoint2
  , deviceCustomFloatingPoint3
  , deviceCustomFloatingPoint4
  , deviceCustomNumber1
  , deviceCustomNumber2
  , deviceCustomNumber3
  , deviceCustomNumber4
  , baseEventCount
  , deviceCustomString1
  , deviceCustomString2
  , deviceCustomString3
  , deviceCustomString4
  , deviceCustomString5
  , deviceCustomString6
  , destinationDnsDomain
  , destinationServiceName
  , destinationTranslatedAddress
  , destinationTranslatedPort
  , deviceCustomDate1
  , deviceCustomDate2
  , deviceDirectionInbound
  , deviceDirectionOutbound
  , deviceDnsDomain
  , deviceExternalId
  , deviceFacility
  , deviceInboundInterface
  , deviceMacAddress
  , deviceNtDomain
  , deviceOutboundInterface
  , deviceProcessName
  , deviceTranslatedAddress
  , destinationHostName
  , destinationMacAddress
  , destinationNtDomain
  , destinationProcessId
  , destinationUserPrivileges
  , destinationProcessName
  , destinationPort
  , destinationAddress
  , destinationUserId
  , destinationUserName
  , deviceAddress
  , deviceHostName
  , deviceProcessId
  , endTime
  , externalId
  , fileCreateTime
  , fileHash
  , fileId
  , fileModificationTime
  , filePath
  , filePermission
  , fileType
  , fileName
  , fileSize
  , bytesIn
  , message
  , oldFileCreateTime
  , oldFileHash
  , oldFileId
  , oldFileModificationTime
  , oldFileName
  , oldFilePath
  , oldFilePermission
  , oldFileSize
  , oldFileType
  , bytesOut
  , eventOutcome
  , transportProtocol
  , reason
  , requestURL
  , requestClientApplication
  , requestCookies
  , requestMethod
  , receiptTime
  , sourceHostName
  , sourceMacAddress
  , sourceNtDomain
  , sourceDnsDomain
  , sourceServiceName
  , sourceTranslatedAddress
  , sourceTranslatedPort
  , sourceProcessId
  , sourceUserPrivileges
  , sourceProcessName
  , sourcePort
  , sourceAddress
  , startTime
  , sourceUserId
  , sourceUserName
  ) where

--------------------------------------------------------------------------------
import           Data.ByteString.Builder
import           Data.Monoid             (Monoid, mempty)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Semigroup          (Semigroup, (<>))
--------------------------------------------------------------------------------

data Extensions = Extensions { unExtensions :: Builder }
                | NoExtension

instance Semigroup Extensions where
  Extensions a <> Extensions b = Extensions $ a <> " " <> b
  NoExtension  <> b            = b
  a            <> NoExtension  = a

instance Monoid Extensions where
  mempty                                = emptyExtensions
  {-# INLINE mappend #-}
  mappend                               = (<>)

emptyExtensions :: Extensions
emptyExtensions = NoExtension

-- |
-- >>> :set -XOverloadedStrings
-- >>> toLazyByteString $ extensionsBuilder (applicationProtocol "PUT" <> deviceCustomIPv6Address1 "localnet" "::1")
-- "app=PUT c6a1Label=localnet c6a1=::1"
extensionsBuilder :: Extensions -> Builder
extensionsBuilder (Extensions b) = b
extensionsBuilder NoExtension    = mempty

{-# INLINE ext #-}
ext :: T.Text -> Builder -> Extensions
ext k v = Extensions (T.encodeUtf8Builder k <> "=" <> v)

-- | See /Chapter 4: User-Defined Extensions/ from the reference
customExtension :: T.Text -> T.Text -> Extensions
customExtension k = ext k . renderExtVal

renderExtVal :: T.Text -> Builder
renderExtVal = T.encodeUtf8Builder . T.concatMap go
  where go '\\' = "\\\\"
        go '=' = "\\="
        go '\r' = "\\r"
        go '\n' = "\\n"
        go i = T.singleton i

type IPv6Address = T.Text
type IPv4Address = T.Text
type MACAddress  = T.Text
type TimeStamp   = UTCTime

utcTimeBuilder :: UTCTime -> Builder
utcTimeBuilder = intDec . fromInteger . round . (* 1000) . utcTimeToPOSIXSeconds

-- Max: 63 Character
deviceAction :: T.Text -> Extensions
deviceAction = customExtension "act"

-- Max: 31 Character
applicationProtocol :: T.Text -> Extensions
applicationProtocol = customExtension "app"

deviceCustomIPv6Address1, deviceCustomIPv6Address2, deviceCustomIPv6Address3, deviceCustomIPv6Address4
  :: T.Text -> IPv6Address -> Extensions
deviceCustomIPv6Address1 t a = customExtension "c6a1Label" t <> customExtension "c6a1" a
deviceCustomIPv6Address2 t a = customExtension "c6a2Label" t <> customExtension "c6a2" a
deviceCustomIPv6Address3 t a = customExtension "c6a3Label" t <> customExtension "c6a3" a
deviceCustomIPv6Address4 t a = customExtension "c6a4Label" t <> customExtension "c6a4" a

-- Max: 1023 Character
deviceEventCategory :: T.Text -> Extensions
deviceEventCategory = customExtension "cat"

deviceCustomFloatingPoint1, deviceCustomFloatingPoint2, deviceCustomFloatingPoint3, deviceCustomFloatingPoint4
  :: T.Text -> Double -> Extensions
deviceCustomFloatingPoint1 t a = customExtension "cfp1Label" t <> ext "cfp1" (doubleDec a)
deviceCustomFloatingPoint2 t a = customExtension "cfp2Label" t <> ext "cfp2" (doubleDec a)
deviceCustomFloatingPoint3 t a = customExtension "cfp3Label" t <> ext "cfp3" (doubleDec a)
deviceCustomFloatingPoint4 t a = customExtension "cfp4Label" t <> ext "cfp4" (doubleDec a)

-- Max: 1023 Character for labels
deviceCustomNumber1, deviceCustomNumber2, deviceCustomNumber3, deviceCustomNumber4
  :: T.Text -> Int -> Extensions
deviceCustomNumber1 t a = customExtension "cn1Label" t <> ext "cn1" (intDec a)
deviceCustomNumber2 t a = customExtension "cn2Label" t <> ext "cn2" (intDec a)
deviceCustomNumber3 t a = customExtension "cn3Label" t <> ext "cn3" (intDec a)
deviceCustomNumber4 t a = customExtension "cn4Label" t <> ext "cn4" (intDec a)

baseEventCount :: Int -> Extensions
baseEventCount = ext "cnt" . intDec

-- Max: 1023 Character for labels
deviceCustomString1, deviceCustomString2, deviceCustomString3, deviceCustomString4, deviceCustomString5, deviceCustomString6
  :: T.Text -> T.Text -> Extensions
deviceCustomString1 t a = customExtension "cs1Label" t <> customExtension "cs1" a
deviceCustomString2 t a = customExtension "cs2Label" t <> customExtension "cs2" a
deviceCustomString3 t a = customExtension "cs3Label" t <> customExtension "cs3" a
deviceCustomString4 t a = customExtension "cs4Label" t <> customExtension "cs4" a
deviceCustomString5 t a = customExtension "cs5Label" t <> customExtension "cs5" a
deviceCustomString6 t a = customExtension "cs6Label" t <> customExtension "cs6" a

destinationDnsDomain :: T.Text -> Extensions
destinationDnsDomain = customExtension "destinationDnsDomain"

destinationServiceName :: T.Text -> Extensions
destinationServiceName = customExtension "destinationServiceName"

destinationTranslatedAddress :: IPv4Address -> Extensions
destinationTranslatedAddress = customExtension "destinationTranslatedAddress"

destinationTranslatedPort :: Int -> Extensions
destinationTranslatedPort = ext "deviceTranslatedPort" . intDec

-- Max: 1023 Character for labels
deviceCustomDate1, deviceCustomDate2 :: T.Text -> TimeStamp -> Extensions
deviceCustomDate1 t a = customExtension "deviceCustomDate1Label" t <> ext "deviceCustomDate1" (utcTimeBuilder a)
deviceCustomDate2 t a = customExtension "deviceCustomDate2Label" t <> ext "deviceCustomDate2" (utcTimeBuilder a)

deviceDirectionInbound, deviceDirectionOutbound :: Extensions
deviceDirectionInbound  = ext "deviceDirection" (intDec 0)
deviceDirectionOutbound = ext "deviceDirection" (intDec 1)

-- Max: 255 Character
deviceDnsDomain :: T.Text -> Extensions
deviceDnsDomain = customExtension "deviceDnsDomain"

-- Max: 255 Character
deviceExternalId :: T.Text -> Extensions
deviceExternalId = customExtension "deviceExternalId"

-- Max: 1023 Character
deviceFacility :: T.Text -> Extensions
deviceFacility = customExtension "deviceFacility"

-- Max: 15 Character
deviceInboundInterface :: T.Text -> Extensions
deviceInboundInterface = customExtension "deviceInboundInterface"

deviceMacAddress :: MACAddress -> Extensions
deviceMacAddress = customExtension "deviceMacAddress"

-- Max: 255 Character
deviceNtDomain :: T.Text -> Extensions
deviceNtDomain = customExtension "deviceNtDomain"

-- Max: 15 Character
deviceOutboundInterface :: T.Text -> Extensions
deviceOutboundInterface = customExtension "deviceOutboundInterface"

-- Max: 1023 Character
deviceProcessName :: T.Text -> Extensions
deviceProcessName = customExtension "deviceProcessName"

deviceTranslatedAddress :: IPv4Address -> Extensions
deviceTranslatedAddress = customExtension "deviceTranslatedAddress"

-- Max: 1023 Character
destinationHostName :: T.Text -> Extensions
destinationHostName = customExtension "dhost"

destinationMacAddress :: MACAddress -> Extensions
destinationMacAddress = customExtension "dmac"

-- Max: 255 Character
destinationNtDomain :: T.Text -> Extensions
destinationNtDomain = customExtension "dntdom"

destinationProcessId :: T.Text -> Extensions
destinationProcessId = customExtension "dpid"

-- Max: 1023 Character
destinationUserPrivileges :: T.Text -> Extensions
destinationUserPrivileges = customExtension "dpriv"

-- Max: 1023 Character
destinationProcessName :: T.Text -> Extensions
destinationProcessName = customExtension "dproc"

destinationPort :: Int -> Extensions
destinationPort = ext "dpt" . intDec

destinationAddress :: IPv4Address -> Extensions
destinationAddress = customExtension "dst"

-- Max: 1023 Character
destinationUserId :: T.Text -> Extensions
destinationUserId = customExtension "duid"

-- Max: 1023 Character
destinationUserName :: T.Text -> Extensions
destinationUserName = customExtension "duser"

deviceAddress :: IPv4Address -> Extensions
deviceAddress = customExtension "dvc"

-- Max: 100 Character
deviceHostName :: T.Text -> Extensions
deviceHostName = customExtension "dvchost"

deviceProcessId :: T.Text -> Extensions
deviceProcessId = customExtension "dvcpid"

endTime :: TimeStamp -> Extensions
endTime = ext "end" . utcTimeBuilder

-- Max: 40 Character
externalId :: T.Text -> Extensions
externalId = customExtension "externalId"

fileCreateTime :: TimeStamp -> Extensions
fileCreateTime = ext "fileCreateTime" . utcTimeBuilder

-- Max: 255 Character
fileHash :: T.Text -> Extensions
fileHash = customExtension "fileHash"

-- Max: 1023 Character
fileId :: T.Text -> Extensions
fileId = customExtension "fileId"

fileModificationTime :: TimeStamp -> Extensions
fileModificationTime = ext "fileModificationTime" . utcTimeBuilder

-- Max: 1023 Character
filePath :: T.Text -> Extensions
filePath = customExtension "filePath"

-- Max: 1023 Character
filePermission :: T.Text -> Extensions
filePermission = customExtension "filePermission"

-- Max: 1023 Character
fileType :: T.Text -> Extensions
fileType = customExtension "fileType"

-- Max: 1023 Character
fileName :: T.Text -> Extensions
fileName = customExtension "fileName"

fileSize :: Int -> Extensions
fileSize = ext "fileSize" . intDec

bytesIn :: Int -> Extensions
bytesIn = ext "in" . intDec

-- Max: 1023 Character
message :: T.Text -> Extensions
message = customExtension "msg"

oldFileCreateTime :: TimeStamp -> Extensions
oldFileCreateTime = ext "oldFileCreateTime" . utcTimeBuilder

-- Max: 255 Character
oldFileHash :: T.Text -> Extensions
oldFileHash = customExtension "oldFileHash"

-- Max: 1023 Character
oldFileId :: T.Text -> Extensions
oldFileId = customExtension "oldFileId"

oldFileModificationTime :: TimeStamp -> Extensions
oldFileModificationTime = ext "oldFileModificationTime" . utcTimeBuilder

-- Max: 1023 Character
oldFileName :: T.Text -> Extensions
oldFileName = customExtension "oldFileName"

-- Max: 1023 Character
oldFilePath :: T.Text -> Extensions
oldFilePath = customExtension "oldFilePath"

-- Max: 1023 Character
oldFilePermission :: T.Text -> Extensions
oldFilePermission = customExtension "oldFilePermission"

-- Max: 1023 Character
oldFileSize :: Int -> Extensions
oldFileSize = ext "oldFileSize" . intDec

-- Max: 1023 Character
oldFileType :: T.Text -> Extensions
oldFileType = customExtension "oldFileType"

bytesOut :: Int -> Extensions
bytesOut = ext "out" . intDec
-- Max: 63 Character
eventOutcome :: T.Text -> Extensions
eventOutcome = customExtension "outcome"

transportProtocol :: T.Text -> Extensions
transportProtocol = customExtension "proto"

-- Max: 1023 Character
reason :: T.Text -> Extensions
reason = customExtension "reason"

-- Max: 1023 Character
requestURL :: T.Text -> Extensions
requestURL = customExtension "request"

-- Max: 1023 Character
requestClientApplication :: T.Text -> Extensions
requestClientApplication = customExtension "requestClientApplication"

-- Max: 1023 Character
requestCookies :: T.Text -> Extensions
requestCookies = customExtension "requestCookies"

-- Max: 1023 Character
requestMethod :: T.Text -> Extensions
requestMethod = customExtension "requestMethod"

receiptTime :: TimeStamp -> Extensions
receiptTime = ext "rt" . utcTimeBuilder

-- Max: 1023 Character
sourceHostName :: T.Text -> Extensions
sourceHostName = customExtension "shost"

sourceMacAddress :: MACAddress -> Extensions
sourceMacAddress = customExtension "smac"

-- Max: 255 Character
sourceNtDomain :: T.Text -> Extensions
sourceNtDomain = customExtension "sntdom"

-- Max: 255 Character
sourceDnsDomain :: T.Text -> Extensions
sourceDnsDomain = customExtension "sourceDnsDomain"

-- Max: 1023 Character
sourceServiceName :: T.Text -> Extensions
sourceServiceName = customExtension "sourceServiceName"

sourceTranslatedAddress :: IPv4Address -> Extensions
sourceTranslatedAddress = customExtension "sourceTranslatedAddress"

sourceTranslatedPort :: Int -> Extensions
sourceTranslatedPort = ext "sourceTranslatedPort" . intDec

sourceProcessId :: Int -> Extensions
sourceProcessId = ext "spid" . intDec

-- Max: 1023 Character
sourceUserPrivileges :: T.Text -> Extensions
sourceUserPrivileges = customExtension "spriv"

-- Max: 1023 Character
sourceProcessName :: T.Text -> Extensions
sourceProcessName = customExtension "sproc"

sourcePort :: Int -> Extensions
sourcePort = ext "spt" . intDec

sourceAddress :: IPv4Address -> Extensions
sourceAddress = customExtension "src"

startTime :: TimeStamp -> Extensions
startTime = ext "start" . utcTimeBuilder

-- Max: 1023 Character
sourceUserId :: T.Text -> Extensions
sourceUserId = customExtension "suid"

-- Max: 1023 Character
sourceUserName :: T.Text -> Extensions
sourceUserName = customExtension "suser"


-- TODO: enforce data size limits for each extension
