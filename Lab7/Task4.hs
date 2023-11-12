import System.IO
import System.Environment

main=do
     (fnam:ns:etc) <- getArgs
     handle <- openFile fnam ReadMode
     contf <- hGetContents handle
     mapM putStrLn (take (read ns) (lines contf))
     hClose handle
