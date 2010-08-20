-- http://haskell.org/haskellwiki/?title=Xmonad/Config_archive/Brent_Yorgey%27s_xmonad.hs
import System.IO
import System.Time
import System.Process

main :: IO ()
main = do hSetBuffering stdout LineBuffering
          inp <- getLine
          stat <- mkStatus inp
          putStrLn stat
          main

mkStatus :: String -> IO String
mkStatus inp = do time    <- getTime
                  bat     <- getBat
                  loadAvg <- getLoadAvg
                  return . concat $ [ inp, s
                                    , time, s
                                    , bat, s
                                    , loadAvg ]
    where s = " | "
       
getTime :: IO String
getTime = do cal <- (getClockTime >>= toCalendarTime)
             return $ concat 
                        [ show . (1+) . fromEnum . ctMonth $ cal
                        , "/", show . ctDay $ cal
                        , "  ", show . fromMilitary . ctHour $ cal
                        , ":", padMin . ctMin $ cal
                        , " ", ampm . ctHour $ cal ]
  where
    fromMilitary 0 = 12
    fromMilitary h | h > 12    = h - 12
                   | otherwise = h
    padMin m | m < 10    = '0' : (show m)
             | otherwise = show m
    ampm h | h <  12   = "AM"
           | otherwise = "PM"

getBat :: IO String
getBat = do (_, out, _, proc) <- runInteractiveCommand "/usr/bin/acpi | sed -r 's/.*?: (.*%).*/\\1/; s/discharging, ([0-9]+%)/\\1-/; s/charging, ([0-9]+%)/\\1+/; s/charged, //'"
            batInfo <- hGetLine out
            waitForProcess proc
            return batInfo

getLoadAvg :: IO String
getLoadAvg = do (_, out, _, proc) <- runInteractiveCommand "/usr/bin/uptime | sed 's/.*: //; s/,//g'"
                uptimeInfo <- hGetLine out
                waitForProcess proc
                return uptimeInfo
