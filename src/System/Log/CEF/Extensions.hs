{-# LANGUAGE OverloadedStrings #-}

module System.Log.CEF.Extensions
  ( Extensions
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
import           Data.Function
import           Data.List               (intersperse)
import           Data.Monoid
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
--------------------------------------------------------------------------------

newtype Extensions = Extensions { unExtensions :: Builder }

instance Monoid Extensions where
  mempty                                = Extensions mempty
  {-# INLINE mappend #-}
  Extensions e1 `mappend` Extensions e2 = Extensions (e1 <> " " <> e2)

-- |
-- >>> :set -XOverloadedStrings
-- >>> toLazyByteString $ extensionsBuilder (applicationProtocol "PUT" <> deviceCustomIPv6Address1 "localnet" "::1")
-- "app=PUT c6a1Label=localnet c6a1=::1"
extensionsBuilder :: Extensions -> Builder
extensionsBuilder = unExtensions

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
type TimeStamp   = T.Text

deviceAction :: T.Text -> Extensions
deviceAction = customExtension "act"

applicationProtocol :: T.Text -> Extensions
applicationProtocol = customExtension "app"

deviceCustomIPv6Address1, deviceCustomIPv6Address2, deviceCustomIPv6Address3, deviceCustomIPv6Address4
  :: T.Text -> IPv6Address -> Extensions
deviceCustomIPv6Address1 t a = customExtension "c6a1Label" t <> customExtension "c6a1" a
deviceCustomIPv6Address2 t a = customExtension "c6a2Label" t <> customExtension "c6a2" a
deviceCustomIPv6Address3 t a = customExtension "c6a3Label" t <> customExtension "c6a3" a
deviceCustomIPv6Address4 t a = customExtension "c6a4Label" t <> customExtension "c6a4" a

deviceEventCategory :: T.Text -> Extensions
deviceEventCategory = customExtension "cat"

deviceCustomFloatingPoint1, deviceCustomFloatingPoint2, deviceCustomFloatingPoint3, deviceCustomFloatingPoint4
  :: T.Text -> Double -> Extensions
deviceCustomFloatingPoint1 t a = customExtension "cfp1Label" t <> ext "cfp1" (doubleDec a)
deviceCustomFloatingPoint2 t a = customExtension "cfp2Label" t <> ext "cfp2" (doubleDec a)
deviceCustomFloatingPoint3 t a = customExtension "cfp3Label" t <> ext "cfp3" (doubleDec a)
deviceCustomFloatingPoint4 t a = customExtension "cfp4Label" t <> ext "cfp4" (doubleDec a)

deviceCustomNumber1, deviceCustomNumber2, deviceCustomNumber3, deviceCustomNumber4
  :: T.Text -> Int -> Extensions
deviceCustomNumber1 t a = customExtension "cn1Label" t <> ext "cn1" (intDec a)
deviceCustomNumber2 t a = customExtension "cn2Label" t <> ext "cn2" (intDec a)
deviceCustomNumber3 t a = customExtension "cn3Label" t <> ext "cn3" (intDec a)
deviceCustomNumber4 t a = customExtension "cn4Label" t <> ext "cn4" (intDec a)

baseEventCount :: Int -> Extensions
baseEventCount = ext "cnt" . intDec

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

deviceCustomDate1, deviceCustomDate2 :: T.Text -> TimeStamp -> Extensions
deviceCustomDate1 t a = customExtension "deviceCustomDate1Label" t <> customExtension "deviceCustomDate1" a
deviceCustomDate2 t a = customExtension "deviceCustomDate2Label" t <> customExtension "deviceCustomDate2" a

deviceDirectionInbound, deviceDirectionOutbound :: Extensions
deviceDirectionInbound  = ext "deviceDirection" (intDec 0)
deviceDirectionOutbound = ext "deviceDirection" (intDec 1)

deviceDnsDomain :: T.Text -> Extensions
deviceDnsDomain = customExtension "deviceDnsDomain"

deviceExternalId :: T.Text -> Extensions
deviceExternalId = customExtension "deviceExternalId"

deviceFacility :: T.Text -> Extensions
deviceFacility = customExtension "deviceFacility"

deviceInboundInterface :: T.Text -> Extensions
deviceInboundInterface = customExtension "deviceInboundInterface"

deviceMacAddress :: MACAddress -> Extensions
deviceMacAddress = customExtension "deviceMacAddress"

deviceNtDomain :: T.Text -> Extensions
deviceNtDomain = customExtension "deviceNtDomain"

deviceOutboundInterface :: T.Text -> Extensions
deviceOutboundInterface = customExtension "deviceOutboundInterface"

deviceProcessName :: T.Text -> Extensions
deviceProcessName = customExtension "deviceProcessName"

deviceTranslatedAddress :: IPv4Address -> Extensions
deviceTranslatedAddress = customExtension "deviceTranslatedAddress"

destinationHostName :: T.Text -> Extensions
destinationHostName = customExtension "dhost"

destinationMacAddress :: MACAddress -> Extensions
destinationMacAddress = customExtension "dmac"

destinationNtDomain :: T.Text -> Extensions
destinationNtDomain = customExtension "dntdom"

destinationProcessId :: T.Text -> Extensions
destinationProcessId = customExtension "dpid"

destinationUserPrivileges :: T.Text -> Extensions
destinationUserPrivileges = customExtension "dpriv"

destinationProcessName :: T.Text -> Extensions
destinationProcessName = customExtension "dproc"

destinationPort :: Int -> Extensions
destinationPort = ext "dpt" . intDec

destinationAddress :: IPv4Address -> Extensions
destinationAddress = customExtension "dst"

destinationUserId :: T.Text -> Extensions
destinationUserId = customExtension "duid"

destinationUserName :: T.Text -> Extensions
destinationUserName = customExtension "duser"

deviceAddress :: IPv4Address -> Extensions
deviceAddress = customExtension "dvc"

deviceHostName :: T.Text -> Extensions
deviceHostName = customExtension "dvchost"

deviceProcessId :: T.Text -> Extensions
deviceProcessId = customExtension "dvcpid"

endTime :: TimeStamp -> Extensions
endTime = customExtension "end"

externalId :: T.Text -> Extensions
externalId = customExtension "externalId"

fileCreateTime :: TimeStamp -> Extensions
fileCreateTime = customExtension "fileCreateTime"

fileHash :: T.Text -> Extensions
fileHash = customExtension "fileHash"

fileId :: T.Text -> Extensions
fileId = customExtension "fileId"

fileModificationTime :: TimeStamp -> Extensions
fileModificationTime = customExtension "fileModificationTime"

filePath :: T.Text -> Extensions
filePath = customExtension "filePath"

filePermission :: T.Text -> Extensions
filePermission = customExtension "filePermission"

fileType :: T.Text -> Extensions
fileType = customExtension "fileType"

fileName :: T.Text -> Extensions
fileName = customExtension "fileName"

fileSize :: Int -> Extensions
fileSize = ext "fileSize" . intDec

bytesIn :: Int -> Extensions
bytesIn = ext "in" . intDec

message :: T.Text -> Extensions
message = customExtension "msg"

oldFileCreateTime :: TimeStamp -> Extensions
oldFileCreateTime = customExtension "oldFileCreateTime"

oldFileHash :: T.Text -> Extensions
oldFileHash = customExtension "oldFileHash"

oldFileId :: T.Text -> Extensions
oldFileId = customExtension "oldFileId"

oldFileModificationTime :: TimeStamp -> Extensions
oldFileModificationTime = customExtension "oldFileModificationTime"

oldFileName :: T.Text -> Extensions
oldFileName = customExtension "oldFileName"

oldFilePath :: T.Text -> Extensions
oldFilePath = customExtension "oldFilePath"

oldFilePermission :: T.Text -> Extensions
oldFilePermission = customExtension "oldFilePermission"

oldFileSize :: Int -> Extensions
oldFileSize = ext "oldFileSize" . intDec

oldFileType :: T.Text -> Extensions
oldFileType = customExtension "oldFileType"

bytesOut :: Int -> Extensions
bytesOut = ext "out" . intDec

eventOutcome :: T.Text -> Extensions
eventOutcome = customExtension "outcome"

transportProtocol :: T.Text -> Extensions
transportProtocol = customExtension "proto"

reason :: T.Text -> Extensions
reason = customExtension "reason"

requestURL :: T.Text -> Extensions
requestURL = customExtension "request"

requestClientApplication :: T.Text -> Extensions
requestClientApplication = customExtension "requestClientApplication"

requestCookies :: T.Text -> Extensions
requestCookies = customExtension "requestCookies"

requestMethod :: T.Text -> Extensions
requestMethod = customExtension "requestMethod"

receiptTime :: TimeStamp -> Extensions
receiptTime = customExtension "rt"

sourceHostName :: T.Text -> Extensions
sourceHostName = customExtension "shost"

sourceMacAddress :: MACAddress -> Extensions
sourceMacAddress = customExtension "smac"

sourceNtDomain :: T.Text -> Extensions
sourceNtDomain = customExtension "sntdom"

sourceDnsDomain :: T.Text -> Extensions
sourceDnsDomain = customExtension "sourceDnsDomain"

sourceServiceName :: T.Text -> Extensions
sourceServiceName = customExtension "sourceServiceName"

sourceTranslatedAddress :: IPv4Address -> Extensions
sourceTranslatedAddress = customExtension "sourceTranslatedAddress"

sourceTranslatedPort :: Int -> Extensions
sourceTranslatedPort = ext "sourceTranslatedPort" . intDec

sourceProcessId :: Int -> Extensions
sourceProcessId = ext "spid" . intDec

sourceUserPrivileges :: T.Text -> Extensions
sourceUserPrivileges = customExtension "spriv"

sourceProcessName :: T.Text -> Extensions
sourceProcessName = customExtension "sproc"

sourcePort :: Int -> Extensions
sourcePort = ext "spt" . intDec

sourceAddress :: IPv4Address -> Extensions
sourceAddress = customExtension "src"

startTime :: TimeStamp -> Extensions
startTime = customExtension "start"

sourceUserId :: T.Text -> Extensions
sourceUserId = customExtension "suid"

sourceUserName :: T.Text -> Extensions
sourceUserName = customExtension "suser"

