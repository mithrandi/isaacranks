module Instrument
  ( requestDuration
  , instrumentApp
  , observeDurationL
  , observeHandler
  , observeHandlerL
  , timeAction
  ) where

import           Data.Ratio ((%))
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import           Data.Text.Encoding.Error
import           Import
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Prometheus as Prom
import qualified System.Clock as Clock

instance Prom.MonadMonitor (HandlerFor site) where
  doIO = liftIO

-- | Core information about HTTP requests:
--
-- Labels:
-- * handler: the name of the application
-- * method: the HTTP method requested
-- * status_code: the HTTP response code
--
-- Actual metric is the latency of the request.
type RequestDuration = Prom.Vector Prom.Label3 Prom.Histogram

requestDuration :: IO RequestDuration
requestDuration =
  Prom.register $ Prom.vector ("handler", "method", "status_code") $ Prom.histogram info Prom.defaultBuckets
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
      Prom.withLabel metric (handler, method, T.pack status) (flip Prom.observe latency)
      where
        method = E.decodeUtf8With lenientDecode (Wai.requestMethod req)
        status = show statusCode

observeDurationL ::
  (MonadIO m, Prom.MonadMonitor m, Prom.Observer metric, Prom.Label l) =>
  Prom.Vector l metric -> l -> m a -> m a
observeDurationL metric label io = do
    (result, duration) <- timeAction io
    liftIO $ Prom.withLabel metric label (flip Prom.observe duration)
    return result


-- | Lifted version of 'Prometheus.timeAction'
timeAction :: MonadIO m => m a -> m (a, Double)
timeAction io = do
    start  <- liftIO $ Clock.getTime Clock.Monotonic
    result <- io
    end    <- liftIO $ Clock.getTime Clock.Monotonic
    let duration = Clock.toNanoSecs (end `Clock.diffTimeSpec` start) % 1000000000
    return (result, fromRational duration)


observeHandler ::
  Prom.Observer metric =>
  (AppMetrics -> metric) -> HandlerFor App a -> HandlerFor App a
observeHandler m h = getsYesod (m . appMetrics) >>= flip Prom.observeDuration h


observeHandlerL ::
  (Prom.Observer metric, Prom.Label l) =>
  (AppMetrics -> Prom.Vector l metric) -> l -> HandlerFor App a -> HandlerFor App a
observeHandlerL m label h = do
  metric <- getsYesod (m . appMetrics)
  observeDurationL metric label h
