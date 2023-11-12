import System.IO
import System.Environment

main=do
     (fnam:etc)<-getArgs
     handle <- openFile fnam ReadMode
     contf <- hGetContents handle
     putStrLn contf
     hClose handle
