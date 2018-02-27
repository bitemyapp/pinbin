{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}

module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)

-- import Yesod.Auth.Dummy

import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import Data.Text (splitOn)

data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

mkYesodData "App" $(parseRoutesFile "config/routes")

newtype UserNameP =
  UserNameP Text
  deriving (Eq, Show, Read)

instance PathPiece UserNameP where
  toPathPiece (UserNameP i) = "u:" <> i
  fromPathPiece s =
    case splitOn ":" s of
      ["u", ""] -> Nothing
      ["u", uname] -> Just $ UserNameP uname
      _ -> Nothing

newtype TagsP =
  TagsP [Text]
  deriving (Eq, Show, Read)

instance PathPiece TagsP where
  toPathPiece (TagsP tags) = "t:" <> (intercalate "+" tags)
  fromPathPiece s =
    case splitOn ":" s of
      ["t", ""] -> Nothing
      ["t", tags] -> Just $ TagsP (splitOn "+" tags)
      _ -> Nothing

instance Yesod App where
    approot = ApprootRequest $ \app req ->
        case appRoot (appSettings app) of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    yesodMiddleware = defaultYesodMiddleware

    defaultLayout widget = do
        master <- getYesod
        -- mmsg <- getMessage

        -- muser <- maybeAuthPair
        -- mcurrentRoute <- getCurrentRoute

        pc <- widgetToPageContent $ do
            addStylesheet (StaticR css_main_css)
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    -- isAuthorized (AuthR _) _ = return Authorized
    -- isAuthorized CommentR _ = return Authorized
    -- isAuthorized HomeR _ = return Authorized
    -- isAuthorized FaviconR _ = return Authorized
    -- isAuthorized RobotsR _ = return Authorized
    -- isAuthorized (StaticR _) _ = return Authorized

    -- isAuthorized ProfileR _ = isAuthenticated

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir (appSettings master)
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action (appConnPool master)
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool


-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- instance YesodAuth App where
--     type AuthId App = UserId

--     -- Where to send a user after successful login
--     loginDest _ = HomeR
--     -- Where to send a user after logout
--     logoutDest _ = HomeR
--     -- Override the above two destinations when a Referer: header is present
--     redirectToReferer _ = True

--     authenticate creds = runDB $ do
--         x <- getBy $ UniqueUser $ credsIdent creds
--         case x of
--             Just (Entity uid _) -> return $ Authenticated uid
--             Nothing -> Authenticated <$> insert User
--                 { userIdent = credsIdent creds
--                 , userPassword = Nothing
--                 }

--     -- You can add other plugins like Google Email, email or OAuth here
--     authPlugins app = [authOpenId Claimed []] ++ extraAuthPlugins
--         -- Enable authDummy login if enabled.
--         where extraAuthPlugins = [authDummy | appAuthDummyLogin $ appSettings app]

--     authHttpManager = getHttpManager

-- -- | Access function to determine if a user is logged in.
-- isAuthenticated :: Handler AuthResult
-- isAuthenticated = do
--     muid <- maybeAuthId
--     return $ case muid of
--         Nothing -> Unauthorized "You must login to access this page"
--         Just _ -> Authorized

-- instance YesodAuthPersist App
