module Instrument
  ( requestDuration
  , instrumentApp
  , observeDuration
  , observeDurationL
  , observeHandler
  , observeHandlerL
  ) where

import qualified Data.ByteString.Char8 as B8
import           Data.Ratio ((%))
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
type RequestDuration = Prom.Metric (Prom.Vector Prom.Label3 Prom.Histogram)

requestDuration :: IO RequestDuration
requestDuration =
  Prom.vector ("handler", "method", "status_code") $ Prom.histogram info Prom.defaultBuckets
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


-- | Lifted version of 'Prometheus.observeDuration'
observeDuration ::
  (MonadIO m, Prom.Observer metric) => Prom.Metric metric -> m a -> m a
observeDuration metric io = do
    (result, duration) <- timeAction io
    liftIO $ Prom.observe duration metric
    return result


observeDurationL ::
  (MonadIO m, Prom.Observer metric, Prom.Label l) =>
  Prom.Metric (Prom.Vector l metric) -> l -> m a -> m a
observeDurationL metric label io = do
    (result, duration) <- timeAction io
    liftIO $ Prom.withLabel label (Prom.observe duration) metric
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
  (Prom.Observer metric, MonadIO m, MonadReader App m) =>
  (AppMetrics -> Prom.Metric metric) -> m b -> m b
observeHandler m h = asks (m . appMetrics) >>= flip observeDuration h


observeHandlerL ::
  (Prom.Observer metric, Prom.Label l, MonadIO m, MonadReader App m) =>
  (AppMetrics -> Prom.Metric (Prom.Vector l metric)) -> l -> m b -> m b
observeHandlerL m label h = do
  metric <- asks (m . appMetrics)
  observeDurationL metric label h
