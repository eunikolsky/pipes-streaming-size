#!/usr/bin/env stack
-- stack --resolver lts-19.6 script --package bytestring,mtl,pipes,pipes-bytestring,pipes-http

import Control.Monad.State.Strict
import Pipes
import Pipes.HTTP
import qualified Data.ByteString as BL
import qualified Pipes.ByteString as PB

type Size = Int

-- | Prints the total size of the downloaded stream so far when each new piece
-- is received.
printTotalSize :: Pipe BL.ByteString BL.ByteString (StateT Size IO) ()
printTotalSize = forever $ do
  bs <- await

  modify' (+ (BL.length bs))
  size <- get
  liftIO $ print size

  yield bs

saveStream :: String -> IO ()
saveStream url = do
  manager <- newManager tlsManagerSettings
  req <- parseUrlThrow url

  withHTTP req manager $ \resp ->
    flip evalStateT 0 $ runEffect
      $ responseBody resp
      >-> printTotalSize
      >-> PB.stdout

main :: IO ()
main = saveStream "https://httpbin.org/drip?duration=2&numbytes=4&code=200&delay=0"
