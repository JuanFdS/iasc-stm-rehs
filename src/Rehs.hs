------------------------------------------------------------
-- Main definitions.
-- This modules defines a Table and its operations
-- Notice that all functions in this module return STM's
------------------------------------------------------------

module Rehs where

import System.Posix.Unistd
import GHC.Conc
import Data.Char (toUpper)
import Control.Concurrent.STM
import Data.Map

data ListaLoca a = Cabeza (TVar a) | Cola (ListaLoca a)
type TablePosta' = ListaLoca (String, String)
type TablePosta = Map String String
type Table = TVar TablePosta
type SlotTransaction a = Table -> STM a

newTable :: STM Table
newTable =  newTVar empty

updateAndReadSlot :: SlotTransaction String -> Table -> STM String
updateAndReadSlot transaction table = transaction table

setTransaction :: String -> String -> SlotTransaction String
setTransaction key value table = do
    modifyTVar table (insert key value)
    pure value

clearTransaction :: String -> SlotTransaction String
clearTransaction key table = do
    modifyTVar table (delete key)
    pure ("Key " ++ key ++ " borrada, gil")

readTransaction :: String -> SlotTransaction String
readTransaction key = \table -> (leemeElMaybe . Data.Map.lookup key) <$> (readTVar table)

clearAllTransaction :: SlotTransaction String
clearAllTransaction = \table -> writeTVar table empty >> pure "Limpiadosky"

upcaseTransaction :: String -> SlotTransaction String
upcaseTransaction = modifyKeyTransaction uppercase

reverseTransaction :: String -> SlotTransaction String
reverseTransaction = modifyKeyTransaction reverse

modifyKeyTransaction :: (String -> String) -> String -> SlotTransaction String
modifyKeyTransaction f key = \table -> modifyTVar table (adjust f key) >> readTransaction key table

withTimeout :: Int -> SlotTransaction String -> SlotTransaction String
withTimeout timeout f = \table -> wait timeout >> f table

reverseWithSleep :: String -> SlotTransaction String
reverseWithSleep key = \table -> wait 3 >> reverseTransaction key table

upcaseWithSleep :: String -> SlotTransaction String
upcaseWithSleep key = \table -> upcaseTransaction key table >> wait 10 >> readTransaction key table

wait timeout = unsafeIOToSTM (threadDelay $ timeout * 1000000)

leemeElMaybe Nothing = "No lo encontre"
leemeElMaybe (Just a) = a

uppercase = fmap toUpper
