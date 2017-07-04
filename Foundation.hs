module Foundation where

import           Database.Persist.Sql (ConnectionPool, runSqlPool)
import           Import.NoFoundation
import           Model.IsaacVersion
import           Network.Wai.Middleware.Prometheus (metricsApp)
import           Settings.Development (development)
import           Text.Hamlet (hamletFile)
import           Text.Jasmine (minifym)
import qualified Web.ClientSession as WC
import           Yesod.Core.Types (Logger)
import           Yesod.EmbeddedStatic (EmbeddedStatic, embedStaticContent)

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
  { appSettings    :: AppSettings
  , appStatic      :: EmbeddedStatic -- ^ Settings for static file serving.
  , appConnPool    :: ConnectionPool -- ^ Database connection pool.
  , appHttpManager :: Manager
  , appLogger      :: Logger
  , appBallotKey   :: WC.Key
  }

instance HasHttpManager App where
    getHttpManager = appHttpManager

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootRequest $ \app req ->
      (fromMaybe (getApprootText guessApproot app req)
        (appRoot $ appSettings app))

    makeSessionBackend _ = return Nothing

    yesodMiddleware = sslOnlyMiddleware 31536000 . simpleVaryMiddleware
      where simpleVaryMiddleware handler = do
              addHeader "Vary" "Accept"
              handler

    defaultLayout widget = do
        master <- getYesod
        --mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheetRemote "https://static.isaacranks.com/styles/bootstrap-3.3.5"
            addStylesheetRemote "https://static.isaacranks.com/styles/bootstrap-theme-3.3.5"
            addStylesheetRemote "https://static.isaacranks.com/styles/icons-4"
            addStylesheetRemote "https://static.isaacranks.com/fa-4.4.0/styles/font-awesome"
            addStylesheet $ StaticR css_isaacranks_css
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- Routes not requiring authentication.
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    -- Default to Authorized for now.
    isAuthorized _ _ = return Authorized

    addStaticContent = embedStaticContent appStatic StaticR mini
      where mini = if development then Right else minifym

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelInfo
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

getMetricsSub :: App -> WaiSubsite
getMetricsSub _ = WaiSubsite metricsApp
