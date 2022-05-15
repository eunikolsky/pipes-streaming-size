#!/usr/bin/env stack
-- stack --resolver lts-19.6 script --package bytestring,mtl,pipes,pipes-bytestring,pipes-http

import Control.Monad.State.Strict
import Pipes
import Pipes.HTTP
import qualified Data.ByteString as BL
import qualified Pipes.ByteString as PB
import qualified Pipes.Prelude as P

type Size = Int

type SizeState = (Size, BL.ByteString)

scanTotalSize :: SizeState -> BL.ByteString -> IO SizeState
scanTotalSize (size, _) bs = let newSize = size + (BL.length bs)
  in print newSize >> pure (newSize, bs)

saveStream :: String -> IO ()
saveStream url = do
  manager <- newManager tlsManagerSettings
  req <- parseUrlThrow url

  withHTTP req manager $ \resp ->
    runEffect
      $ responseBody resp
      >-> P.scanM scanTotalSize (pure (0, BL.empty)) (pure . snd)
      >-> PB.stdout

main :: IO ()
main = saveStream "https://httpbin.org/drip?duration=2&numbytes=4&code=200&delay=0"
