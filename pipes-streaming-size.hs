#!/usr/bin/env stack
-- stack --resolver lts-19.6 script --package bytestring,mtl,pipes,pipes-bytestring,pipes-http

import Control.Monad.State.Strict
import Pipes
import Pipes.HTTP
import qualified Data.ByteString as BL
import qualified Pipes.ByteString as PB

type Size = Int

printTotalSize :: Pipe BL.ByteString BL.ByteString IO ()
printTotalSize = iter 0
  where
    iter :: Size -> Pipe BL.ByteString BL.ByteString IO ()
    iter size = do
      bs <- await

      let newSize = size + (BL.length bs)
      liftIO $ print newSize

      yield bs
      iter newSize

saveStream :: String -> IO ()
saveStream url = do
  manager <- newManager tlsManagerSettings
  req <- parseUrlThrow url

  withHTTP req manager $ \resp ->
    runEffect
      $ responseBody resp
      >-> printTotalSize
      >-> PB.stdout

main :: IO ()
main = saveStream "https://httpbin.org/drip?duration=2&numbytes=4&code=200&delay=0"
