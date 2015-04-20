{-# LANGUAGE OverloadedStrings #-}

module Example where


--------------------------------------------------------------------------------
import           Data.Monoid
import           System.Log.CEF
--------------------------------------------------------------------------------

picusEvent = CEFEvent "Picus" "Picus" "4.0"
assessmentEvent = picusEvent "244027" "Win32 trojan communication" 10 mempty
