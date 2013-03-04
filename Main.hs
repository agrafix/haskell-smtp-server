module Main (main) where

import Smtp

import Control.Concurrent (forkIO)

main =
    do mailChan <- newEmailChan
       forkIO $ recvLoop mailChan
       runServer 25 mailChan

recvLoop chan =
    do email <- getNextEmail chan
       putStrLn "==================="
       putStrLn "New Mail:"
       putStrLn "==================="
       putStrLn $ show email
       recvLoop chan
