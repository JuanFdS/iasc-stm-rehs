------------------------------------------------------------
-- Command parsing functions
-- This module declares functions for parsing the user input
-- and converting it into Slot Transaction - functions that
-- return an STM ().
------------------------------------------------------------

module Rehs.Commands (
    parseSlotTransactionLine) where

import Data.List.Split (splitOn)
import Rehs

type Command = [String]

parseSlotTransactionLine :: String -> SlotTransaction String
parseSlotTransactionLine = parseSlotTransactionCommand . splitOn ":"

parseSlotTransactionCommand :: Command -> SlotTransaction String
parseSlotTransactionCommand ["set", key, value] = Rehs.setTransaction key value
parseSlotTransactionCommand ["clear", key]      = Rehs.clearTransaction key
parseSlotTransactionCommand ["read", key]       = Rehs.readTransaction key
parseSlotTransactionCommand ["clear_all"]       = Rehs.clearAllTransaction
parseSlotTransactionCommand ["reverse", key]       = Rehs.reverseTransaction key
parseSlotTransactionCommand ["upcase", key]       = Rehs.upcaseTransaction key
parseSlotTransactionCommand ("withTimeout":timeout:args)       = Rehs.withTimeout (read timeout) (parseSlotTransactionCommand args)
parseSlotTransactionCommand ["reverseWithSleep", key]       = Rehs.reverseWithSleep key
parseSlotTransactionCommand ["upcaseWithSleep", key]       = Rehs.upcaseWithSleep key
parseSlotTransactionCommand (comando:args)       = \_ -> pure ("No entiendo el comando " ++ comando)

