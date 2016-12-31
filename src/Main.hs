{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main (main) where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (threadDelay)
import Control.Monad (liftM, void, forM_, when)
import Control.Monad.Trans
import Data.IORef
import Data.Maybe (fromJust)
import Data.Monoid
import Data.Time.Clock
import Data.Typeable
import Network.Wai.Middleware.Static
import System.Directory ( createDirectory, createDirectoryIfMissing
                        , doesFileExist, doesDirectoryExist)
import System.FilePath
import System.Posix.Files (createSymbolicLink, setFileMode)
import Text.Blaze.Html (Html, preEscapedText)
import Web.Spock
import Web.Spock.Config
import qualified Data.Text as T
import qualified Data.Text.IO as T

import DataAnalysis hiding (Html)

import Views.Home
import Views.Site
import Views.Report
import Utils
import Report

-- only allow submitting a new report every X seconds
_RATE_LIMIT_ :: NominalDiffTime
_RATE_LIMIT_ = 6

data Session = Session
  { sess_userId :: T.Text
  , sess_lastReport :: UTCTime
  } deriving (Show)

data AppState = AppState
  { app_templates :: [FilePath]
  , app_absoluteTemplateDir :: FilePath
  } deriving (Show)

type CoerciveApp ctx = SpockCtxM ctx () (Maybe Session) AppState ()
type CoerciveAction ctx = SpockActionCtx ctx () (Maybe Session) AppState

tmpDir :: FilePath
tmpDir = "tmp"

templateDir :: FilePath
templateDir = "template"

main :: IO ()
main =
  do createDirectoryIfMissing True tmpDir
     templates <- listDir "template"
     absTmplDir <- absolutize templateDir
     spockCfg <- defaultSpockCfg Nothing PCNoDatabase (AppState
       { app_templates = templates
       , app_absoluteTemplateDir = absTmplDir
       })
     runSpock 7777 (spock spockCfg app)

prepareUserDir :: FilePath -> CoerciveAction ctx ()
prepareUserDir dir =
  do liftIO $ createDirectory dir
     liftIO $ setFileMode dir 0o777
     app <- getState
     -- create symlinks to shared datasets
     forM_ (app_templates app) $ \name ->
       liftIO $ createSymbolicLink (app_absoluteTemplateDir app </> name)
                                          (dir </> name)

session :: CoerciveAction ctx Session
session =
  do sess <- readSession
     case sess of
       Just s ->
         do let dir = tmpDir </> T.unpack (sess_userId s)
            exists <- liftIO $ doesDirectoryExist dir
            -- re-create user dir if it was deleted by the cleanup script
            when (not exists) $ prepareUserDir dir
            return s
       Nothing ->
         do id <- liftIO $ randomStr 30
            let dir = tmpDir </> T.unpack id
            prepareUserDir dir
            now <- liftIO $ getCurrentTime
            let !s =  Session { sess_userId = id, sess_lastReport = now }
            writeSession (Just s)
            return s

sessionId :: CoerciveAction ctx T.Text
sessionId = sess_userId <$> session

sessionLastReportTime :: CoerciveAction ctx T.Text
sessionLastReportTime = sess_userId <$> session

sessionTmpDir :: CoerciveAction ctx FilePath
sessionTmpDir = (tmpDir </>) . T.unpack <$> sessionId

sessionTmpDirUrl :: CoerciveAction ctx FilePath
sessionTmpDirUrl = ("/tmp" </>) . T.unpack <$> sessionId

mkSite :: Html -> CoerciveAction ctx ()
mkSite = blaze . siteView

loadUserFiles :: String -> CoerciveAction ctx [(T.Text, T.Text)]
loadUserFiles desiredExt =
  do dir <- sessionTmpDir
     url <- sessionTmpDirUrl
     entries <- liftIO $ listDir dir
     return $ [ (T.pack base, T.pack (url </> entry))
               | entry <- entries
               , let (base, ext) = splitExtension entry
               , ext == desiredExt ]

app :: SpockM () (Maybe Session) AppState ()
app =
  do middleware $ staticPolicy $ addBase "static"
     middleware $ staticPolicy $ hasPrefix "tmp/"

     get root $
       do sets <- loadUserFiles ".csv"
          reports <- map fst <$> loadUserFiles ".report"
          mkSite $ homeView $ HomeView
            { home_dataSets = sets
            , home_reports = reports
            }

     post "dataset" $ mkSite $ panelWithErrorView "Beta Version" (Just (
        "We are currently in Beta and custom data sets are not yet " <>
        "supported. Feel free to test our service using the provided " <>
        "example data!")) ""

     post "report" $
       do Just (code :: T.Text) <- param "report"
          sess <- session

          now <- liftIO $ getCurrentTime
          let wait = _RATE_LIMIT_ - diffUTCTime now (sess_lastReport sess)
          when (wait > 0) $ liftIO $
            threadDelay (round (realToFrac wait * 1000000.0))

          now2 <- liftIO $ getCurrentTime
          writeSession $ Just $ sess { sess_lastReport = now2 }
          let code' = T.replace "\r\n" "\n" code
          name <- liftIO $ randomStr 30
          dir <- sessionTmpDir
          let reportFile = dir </> (T.unpack name) ++ ".report"
          liftIO $ createAndWriteFile reportFile code'
          redirect $ "/report/" <> name

     get ("report" <//> "new") $ mkSite $ createReportView

     get ("report" <//> var) $ \name ->
       do when (T.any (=='/') name) $
            fail "Path traversal?"
          dir <- sessionTmpDir
          userId <- T.unpack <$> sessionId
          let reportFile = dir </> (T.unpack name) ++ ".report"
              cacheFile = reportFile ++ ".html"
          code <- liftIO $ T.readFile reportFile
          haveCache <- liftIO $ doesFileExist cacheFile
          html <-
            if haveCache
             then liftIO $ T.readFile cacheFile
             else
               do res <- liftIO $ renderReport code (RenderConfig
                    { render_tmpDir = tmpDir
                    , render_userId = userId
                    })
                  liftIO $ createAndWriteFile cacheFile res
                  return res
          mkSite $ reportView $ ReportView
            { report_rendered = html
            , report_name = name
            , report_code = code
            }
