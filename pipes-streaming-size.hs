#!/usr/bin/env stack
-- stack --resolver lts-19.6 script --package pipes,pipes-bytestring,pipes-http

import Pipes
import Pipes.HTTP
import qualified Pipes.ByteString as PB

saveStream :: String -> IO ()
saveStream url = do
  manager <- newManager tlsManagerSettings
  req <- parseUrlThrow url

  withHTTP req manager $ \resp ->
    runEffect
      $ responseBody resp
      >-> PB.stdout

main :: IO ()
main = saveStream "https://httpbin.org/drip?duration=2&numbytes=4&code=200&delay=0"
