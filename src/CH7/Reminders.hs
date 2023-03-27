module CH7.Reminders where

import Control.Concurrent
import Control.Monad
import Text.Printf

setReminders :: IO ()
setReminders =
  forever $ do
    s <- getLine
    forkIO $ setReminder s

setReminder :: String -> IO ()
setReminder s = do
  let t = read s :: Int
  printf "Ok, I'll remind you in %d seconds\n" t
  threadDelay (10^6 * t)                   -- 3
  printf "%d seconds is up! BING!\BEL\n" t -- 4

