{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Web.SimpleStaticServer (runServer) where

import Network.Wai (
  Request, Response, ResponseReceived,
  responseLBS, requestMethod, responseFile,
  pathInfo,
  strictRequestBody
  )
import Network.HTTP.Types.Status (ok200, notFound404, methodNotAllowed405)
import Network.Wai.Handler.Warp (run, Port)
import Network.HTTP.Types.Method (methodHead, methodGet, methodDelete, methodPut, methodPost)
import qualified Data.ByteString.Lazy.Char8 as LazyByteString
import qualified Data.Text as Text
import System.FilePath ((</>))
import System.Directory (doesFileExist, removeFile)
import Data.Semigroup ((<>))
import System.IO.AtomicFileOps (atomicReplaceFile)

data PathInfo = PathInfo { getFileName :: FilePath, getFullPath :: FilePath }

runServer :: Port -> FilePath -> IO ()
runServer port filepath = run port (app filepath)

app :: FilePath -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app basedir req respF = resp >>= respF where
  resp =
    let
      method = requestMethod req
      eqMethod = (==) method
      pathList = pathInfo req

      checkValid :: (PathInfo -> IO Response) -> IO Response
      checkValid f = case pathList of
        [] -> pure $ responseLBS notFound404 [] "This server does not serve root requests"
        fileNameText:rest -> case rest of
          [] ->
            let
              fileName = Text.unpack fileNameText
              fullPath = basedir </> fileName
            in
              f (PathInfo fileName fullPath)
          _ -> pure $ responseLBS notFound404 [] "This server does not serve sub directory requests"

      checkExists :: (PathInfo -> IO Response) -> IO Response
      checkExists f = checkValid go where
        go filePathInfo = do
          let fullPath = getFullPath filePathInfo
          let fileName = getFileName filePathInfo
          fileExists <- doesFileExist fullPath
          case fileExists of
            True -> f filePathInfo
            False -> pure $ responseLBS notFound404 [] ("File: \"" <> LazyByteString.pack fileName <> "\" not found.")
      handleHead = checkExists go where
        go _ = pure $ responseLBS ok200 [] ""
      handleGet = checkExists go where
        go filePathInfo = pure $ responseFile ok200 [] (getFullPath filePathInfo) Nothing
      handleDelete = checkExists go where
        go filePathInfo = do
          removeFile (getFullPath filePathInfo)
          pure $ responseLBS ok200 [] ("File: \"" <> LazyByteString.pack (getFileName filePathInfo) <> "\" deleted.")
      handlePut = checkValid go where
        go filePathInfo = do
          body <- strictRequestBody req
          atomicReplaceFile Nothing (getFullPath filePathInfo) body
          pure $ responseLBS ok200 [] ("File: \"" <> LazyByteString.pack (getFileName filePathInfo) <> "\" written.")
      handlePost = handlePut
      handleUnknown = pure $ responseLBS methodNotAllowed405 [] ("Unknown method " <> LazyByteString.fromStrict method)
    in if
      | eqMethod methodHead   -> handleHead
      | eqMethod methodGet    -> handleGet
      | eqMethod methodDelete -> handleDelete
      | eqMethod methodPut    -> handlePut
      | eqMethod methodPost   -> handlePost
      | otherwise             -> handleUnknown
