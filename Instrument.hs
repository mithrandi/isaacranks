module Instrument
  ( requestDuration
  , instrumentApp
  ) where

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import           Import
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Prometheus as Prom
import qualified System.Clock as Clock


-- | Core information about HTTP requests:
--
-- Labels:
-- * handler: the name of the application
-- * method: the HTTP method requested
-- * status_code: the HTTP response code
--
-- Actual metric is the latency of the request.
type RequestDuration = Prom.Metric (Prom.Vector Prom.Label3 Prom.Summary)

requestDuration :: IO RequestDuration
requestDuration =
  Prom.vector ("handler", "method", "status_code") $ Prom.summary info Prom.defaultQuantiles
  where
    info =
      Prom.Info
        "http_request_duration_seconds"
        "The HTTP request latencies in seconds."

-- | Instrument a WAI app with the default WAI metrics.
instrumentApp
  :: RequestDuration -- ^ The metric to instrument
  -> Text -- ^ The label used to identify this app
  -> Wai.Application -- ^ The app to instrument
  -> Wai.Application -- ^ The instrumented app
instrumentApp metric handler app req resp = do
  start <- Clock.getTime Clock.Monotonic
  app
    req
    (\res -> do
       recordResult start (HTTP.statusCode (Wai.responseStatus res))
       resp res) `onException`
    recordResult start 500
  where
    recordResult start statusCode = do
      end <- Clock.getTime Clock.Monotonic
      let latency = fromRational . (/ 1000000000) . toRational . Clock.toNanoSecs $
            end `Clock.diffTimeSpec` start
      Prom.withLabel (T.unpack handler, method, status) (Prom.observe latency) metric
      where
        method = B8.unpack (Wai.requestMethod req)
        status = show statusCode
