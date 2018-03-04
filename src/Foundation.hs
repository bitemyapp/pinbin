{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, TypeFamilies, TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)

-- import Yesod.Auth.Dummy

import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types
import Yesod.Auth.Message
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

instance PathPiece UserNameP where
  toPathPiece (UserNameP i) = "u:" <> i
  fromPathPiece s =
    case splitOn ":" s of
      ["u", ""] -> Nothing
      ["u", uname] -> Just $ UserNameP uname
      _ -> Nothing

instance PathPiece TagsP where
  toPathPiece (TagsP tags) = "t:" <> (intercalate "+" tags)
  fromPathPiece s =
    case splitOn ":" s of
      ["t", ""] -> Nothing
      ["t", tags] -> Just $ TagsP (splitOn "+" tags)
      _ -> Nothing

instance PathPiece SharedP where
  toPathPiece = \case
    SharedAll -> ""
    SharedPublic -> "public"
    SharedPrivate -> "private"
  fromPathPiece = \case
    "public" -> Just SharedPublic
    "private" -> Just SharedPrivate
    _ -> Nothing

instance PathPiece FilterP where
  toPathPiece = \case
    FilterAll -> ""
    FilterUnread -> "unread"
    FilterUntagged -> "untagged"
    FilterStarred -> "starred"
    FilterSingle bid -> "b:" <> (pack . show) bid
  fromPathPiece = \case
    "unread" -> Just FilterUnread
    "untagged" -> Just FilterUntagged
    "starred" -> Just FilterStarred
    s -> case splitOn ":" s of
        ["b", ""] -> Nothing
        ["b", sbid] ->
          case readMay sbid of
            Just bid -> Just $ FilterSingle bid
            _ -> Nothing
        _ -> Nothing

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action (appConnPool master)

instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance Yesod App where
    approot = ApprootRequest $ \app req ->
        case appRoot (appSettings app) of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        10080 -- min (7 days)
        "config/client_session_key.aes"

    yesodMiddleware = defaultYesodMiddleware

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage
        muser <- maybeAuthPair
        mcurrentRoute <- getCurrentRoute
        pc <- widgetToPageContent $ do
            setTitle "Pinboard-Server"
            addStylesheet (StaticR css_main_css)
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

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
        genFileName lbs = "autogen-" ++ base64md5 lbs

    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError
    makeLogger = return . appLogger

    authRoute _ = Just $ AuthR LoginR
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized _ _ = return Authorized

instance YesodAuth App where
  type AuthId App = UserId
  authHttpManager = getHttpManager
  authPlugins _ = [dbAuthPlugin]
  authenticate = authenticateCreds
  loginDest = const HomeR
  logoutDest = const HomeR
  onLogin = pure ()
  redirectToReferer = const True

instance YesodAuthPersist App

-- dbAuthPlugin

dbAuthPluginName :: Text
dbAuthPluginName = "db"

dbAuthPlugin :: AuthPlugin App
dbAuthPlugin = AuthPlugin dbAuthPluginName dbDispatch dbLoginHandler
  where
    dbDispatch "POST" ["login"] = dbPostLoginR >>= sendResponse
    dbDispatch _ _ = notFound
    dbLoginHandler toParent = do
      setTitle "Pinboard-Server | Log In"
      $(widgetFile "login")

dbLoginR :: AuthRoute
dbLoginR = PluginR dbAuthPluginName ["login"]

dbPostLoginR ::  AuthHandler master TypedContent
dbPostLoginR = do
  mresult <- lift $ runInputPostResult (dbLoginCreds
                    <$> ireq textField "username"
                    <*> ireq textField "password")
  case mresult of
    FormSuccess creds -> lift $ setCredsRedirect creds
    _ -> loginErrorMessageI LoginR InvalidUsernamePass


dbLoginCreds :: Text -> Text -> Creds master
dbLoginCreds username password =
  Creds
  { credsPlugin = dbAuthPluginName
  , credsIdent = username
  , credsExtra = [("password", password)]
  }

authenticateCreds
  :: (AuthId master ~ UserId)
  => Creds master -> Handler (AuthenticationResult App)
authenticateCreds creds =
  runDB $
  authenticatePW
    (credsIdent creds)
    (fromMaybe "" (lookup "password" (credsExtra creds))) >>=
  \case
    Nothing -> pure (UserError InvalidUsernamePass)
    Just (Entity uid _) -> pure (Authenticated uid)

isAuthenticated :: Handler AuthResult
isAuthenticated = maybe (Unauthorized "") (const Authorized) <$> maybeAuthId

maybeAuthUsername :: Handler (Maybe Text)
maybeAuthUsername = runMaybeT $ do
  Entity _ user <- MaybeT maybeAuth
  pure $ userName user

-- util

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger
