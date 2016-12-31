{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Report where

import Control.Applicative ((<$>))
import Control.Exception (catch, finally, IOException)
import Control.Monad (void)
import NeatInterpolation
import System.Directory (removeFile)
import System.Exit (ExitCode(..))
import System.FilePath
import System.Process (system, readProcessWithExitCode)
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Utils

template :: String -> String -> String -> String -> String -> String
template extraPragmas extraImports report tmpDir userId = [string|
    {-# LANGUAGE Safe #-}
    {-# LANGUAGE OverloadedStrings #-}
    {-# LANGUAGE ScopedTypeVariables #-}
    $extraPragmas
    module Main (main) where

    import Control.Applicative ((<$>), (<*>), pure)
    import Data.Text (Text)
    import System.FilePath ((</>))
    import qualified Data.ByteString as BS
    import qualified Data.Text as T
    $extraImports

    import DataAnalysis
    import Utils

    -- user program is polymorphic over m, so that there is no way to lift from IO
    -- Also DataMonad is a newtype, and we expose only Monad/Applicative/Functor
    -- instances
    userprog :: (Monad m) => DataMonad m ()
    userprog = do
      return () -- in case the report is empty, this prevents a syntax error
    $report

    tmpDir :: FilePath
    tmpDir = $tmpDirRepr

    userId :: String
    userId = $userIdRepr

    main :: IO ()
    main =
      do html <- runDataMonad dataMonadCtx userprog
         putStrLn (T.unpack html)
      where
        dataMonadCtx = DataMonadCtx
          { dataMonad_mkTmpFile = mkTmpFile
          , dataMonad_loadUserFile = BS.readFile . (userDir </>)
          }

        mkTmpFile ext storeAction =
          do base <- randomStr 30
             let name = T.unpack base ++ "." ++ ext
             storeAction $ userDir </> name
             return $ T.pack ("/tmp" </> userId </> name)

        userDir = tmpDir </> userId
    |]
  where
    tmpDirRepr = show tmpDir
    userIdRepr = show userId

-- re-indent given lines to the desired depth
reindent :: Int -> [T.Text] -> [T.Text]
reindent desiredDepth lines = map fixLine lines
  where
    indentation = T.length . T.takeWhile (==' ')
    diff = desiredDepth - indentation (lines !! 0)
    fixLine line
      | diff < 0 = T.drop (-diff) line
      | otherwise = T.append (T.replicate diff " ") line

extractPragmas :: [T.Text] -> ([T.Text], T.Text)
extractPragmas lines = (lines', pragma)
  where
    prefix = "lang "
    prefixLen = T.length prefix
    (pragmaLines, lines') = L.partition ((==prefix) . T.take prefixLen) lines
    pragmas = map (T.takeWhile C.isAlphaNum . T.drop prefixLen) pragmaLines
    goodPragmas = filter
        (\pragma -> pragma /= "CPP" && pragma /= "ForeignFunctionInterface") pragmas
    pragma
      | [] <- goodPragmas = ""
      | otherwise = T.concat ["{-# LANGUAGE ", T.intercalate ", " goodPragmas, " #-}"]

extractImports :: [T.Text] -> ([T.Text], T.Text)
extractImports lines = (lines', importStr)
  where
    prefix = "import "
    prefixLen = T.length prefix
    (importLines, lines') = L.partition ((==prefix) . T.take prefixLen) lines

    isImportChar '.' = True
    isImportChar c = C.isAlphaNum c

    isSafeImport str
      | T.take 5 str /= "Data." = False
      | any (`T.isInfixOf` str) ["System", "Control", "GHC", "Unsafe", "IO"] = False
      | otherwise = True
    imports = filter isSafeImport $
              map (T.takeWhile isImportChar . T.drop prefixLen)
              importLines
    importStr = T.intercalate "\n" $ map (T.append "import ") imports

data RenderConfig = RenderConfig
  { render_tmpDir :: FilePath
  , render_userId :: String
  }

dropPrivsTo :: String
dropPrivsTo = "flag"

runConstrained :: [String] -> [String] -> IO (ExitCode, String, String)
runConstrained restrictions cmd = readProcessWithExitCode "prlimit"
  (restrictions ++ ["--"] ++ cmd) ""

runHaskell :: FilePath -> IO (Either T.Text T.Text)
runHaskell fp =
   do (exit, stdout, stderr) <- runConstrained
        [ "--cpu=20"
        , "--rss=131072" -- 512 MiB
        ]
        [ "stack", "ghc", "--"
        , "-XNoForeignFunctionInterface"
        , "-O3", fp, "-isrc", "-o", outfile ]
      case exit of
        ExitSuccess ->
          do --putStrLn $ "Running " ++ outfile
             (exit, stdout, stderr) <- runConstrained
                [ "--nofile=10"
                , "--nproc=10"
                , "--cpu=5" -- seconds
                , "--rss=65536" -- 256MiB
                ]
                ["sudo", "-u", dropPrivsTo, "-g", dropPrivsTo, outfile]
             return $ case exit of
                        ExitSuccess -> Right (T.pack stdout)
                        ExitFailure _ -> Left (T.pack (stdout ++ stderr))
        ExitFailure _ -> return (Left (T.pack stderr))
   where
     outfile = fp ++ ".out"

renderReport :: T.Text -> RenderConfig -> IO T.Text
renderReport report (RenderConfig{..}) =
  do base <- T.unpack <$> randomStr 30
     let code = T.pack $ template (T.unpack pragmas)
                                  (T.unpack imports)
                                  (T.unpack userprog)
                                  render_tmpDir
                                  render_userId
         hsfile = render_tmpDir </> base ++ ".hs"
     {-putStrLn $ T.unpack code-}
     createAndWriteFile hsfile code
     result <- finally (runHaskell hsfile) (cleanup (render_tmpDir </> base))
     return $ case result of
                Left err -> T.concat
                  ["<p>An error occured while creating your report:</p><pre>\n"
                  , err
                  , "\n</pre>"]
                Right res -> res
  where
    lines = T.lines report
    (lines', pragmas) = extractPragmas lines
    (lines'', imports) = extractImports lines'
    userprog = T.unlines (reindent 2 lines'')

    rem name = removeFile name `catch` \(e::IOException) -> return ()
    cleanup base = mapM_ (rem . ((base ++ ".")++))
        [ "o"
        , "hi"
        , "hs.out"
        , "hs"
        ]
