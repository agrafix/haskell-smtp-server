import Network (listenOn, withSocketsDo, accept, PortID(..), Socket)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, hClose, BufferMode(..), Handle)
import Control.Concurrent (forkIO)
import Control.Monad
import Control.Monad.State.Lazy

import qualified Data.Text as T

runServer :: Int -> IO ()
runServer serverPort = withSocketsDo $ do
    let port = fromIntegral serverPort
    sock <- listenOn $ PortNumber port
    putStrLn $ "Haskell SMTP listening on port " ++ (show serverPort)
    sockHandler sock

sockHandler :: Socket -> IO ()
sockHandler sock = do
    (handle, _, _) <- accept sock
    hSetBuffering handle NoBuffering
    forkIO $ newClient handle
    sockHandler sock

type EmailM a = StateT Email IO a

data Email
   = Email
   { mail_client :: String
   , mail_from :: String
   , mail_to :: String
   , mail_read :: Bool
   , mail_data :: String
   } deriving (Show, Eq)

newClient :: Handle -> IO ()
newClient handle =
    do hPutStrLn handle "220 service ready"
       email <- execStateT (commandProcessor handle) (Email "" "" "" False "")
       putStrLn "==================="
       putStrLn "New Mail:"
       putStrLn "==================="
       putStrLn $ show email

respond :: Handle -> Int -> String -> EmailM ()
respond handle code msg = liftIO $ hPutStrLn handle $ (show code) ++ " " ++ msg

respondOK :: Handle -> EmailM ()
respondOK handle = respond handle 250 "OK"

closeHandle :: Handle -> EmailM ()
closeHandle handle =
    do respond handle 221 "closing channel"
       liftIO $ hClose handle

commandProcessor :: Handle -> EmailM ()
commandProcessor handle =
    do line <- liftIO $ hGetLine handle
       liftIO $ putStrLn $ "Client Says: " ++ line
       let cmd = words line
       case cmd of
         ["HELO", client] -> do modify (\s -> s { mail_client = client })
                                respondOK handle
                                commandProcessor handle
         ["MAIL", from] ->  do modify (\s -> s { mail_from = from })
                               respondOK handle
                               commandProcessor handle

         ["RCPT", to] ->  do modify (\s -> s { mail_to = to })
                             respondOK handle
                             commandProcessor handle

         ["DATA"] -> do modify (\s -> s { mail_read = True })
                        respond handle 354 "start mail input"
                        dataProcessor handle

         ["QUIT"] -> do closeHandle handle

         _ -> do respond handle 500 "unknown command"
                 closeHandle handle

dataProcessor :: Handle -> EmailM ()
dataProcessor handle =
    do getData
       respondOK handle
       closeHandle handle
    where
      getData =
          do line <- liftIO $ hGetLine handle
             when ((T.unpack $ T.strip $ T.pack line) /= ".") $
                  do modify (\s -> s { mail_data = (mail_data s) ++ line })
                     getData
