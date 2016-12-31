{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
module Utils
  ( listDir, absolutize, blaze, randomStr, createAndWriteFile )
where

import Control.Exception (finally)
import Control.Monad (liftM)
import Control.Monad.IO.Class
import System.Directory
import System.FilePath ((</>), takeExtension)
import System.IO (hClose)
import System.Posix.IO
import Text.Blaze.Html (Html)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Web.Spock
import qualified Crypto.Random as CR
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T


listDir :: FilePath -> IO [FilePath]
listDir fp = liftM (filter (\(x:xs) -> x /= '.')) $ getDirectoryContents fp

absolutize :: FilePath -> IO FilePath
absolutize = \case
  abs@('/' : _) -> return abs
  rel -> liftM (</> rel) getCurrentDirectory

blaze :: (MonadIO m) => Html -> ActionCtxT ctx m a
blaze html =
  do setHeader "Content-Type" "text/html; charset=utf-8"
     lazyBytes . renderHtml $ html

-- write file only if it does not exist
createAndWriteFile :: FilePath -> T.Text -> IO ()
createAndWriteFile fp txt =
  do h <- openFd fp WriteOnly
                 (Just 0o644)
                 (defaultFileFlags { exclusive = True }) >>= fdToHandle
     T.hPutStr h txt `finally` hClose h

randomStr :: Int -> IO T.Text
randomStr len =
    do by <- CR.getRandomBytes len
       -- make sure the string doesn't start with _ or -, which might be
       -- problematic if passed on the shell
       return $ T.append "A" $
            T.replace "=" "" $ T.replace "/" "_" $ T.replace "+" "-" $
            T.decodeUtf8 $ B64.encode by
