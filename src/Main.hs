-- | This file is part of MySC.
-- |
-- | MySC is free software: you can redistribute it and/or modify
-- | it under the terms of the GNU General Public License as published by
-- | the Free Software Foundation, either version 3 of the License, or
-- | (at your option) any later version.
-- |
-- | MySC is distributed in the hope that it will be useful,
-- | but WITHOUT ANY WARRANTY; without even the implied warranty of
-- | MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- | GNU General Public License for more details.
-- |
-- | You should have received a copy of the GNU General Public License
-- | along with MySC.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           MySC.Common.DB.Types
import           DB

import           Options.Applicative

import           System.Directory
import qualified Data.Text as T
import           Data.Semigroup ((<>))
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Logger
import           Database.Persist hiding (get)
import           Database.Persist.Postgresql hiding (get)
import           Database.Persist.TH
import           Data.HVect
import qualified Data.ByteString as B
import           Data.Time
import           Control.Monad

import           Text.Blaze.Html.Renderer.String
import           Data.Text.Encoding

import           Web.Spock hiding (SessionId)
import           Web.Spock.Config
import           Network.Mime

connStr = "host=localhost dbname=comment user=comment password=comment port=5432"

main = do
  let parser = (,)
        <$> option auto ( short 'p'
                       <> help "port"
                       <> value 8080)
        <*> strOption ( short 'd'
                     <> help "static directory")
  let opts = info (parser <**> helper) fullDesc
  (port, dir) <- execParser opts
  pool <- runNoLoggingT $ createPostgresqlPool connStr 5
  runNoLoggingT $ runSqlPool (runMigration migrateAll) pool
  spockCfg <- defaultSpockCfg Nothing (PCPool pool) connStr
  runSpock port $ spock spockCfg $ commentSystem dir
  
data BlogState
   = BlogState
   { bs_cfg :: BlogCfg
   }

data BlogCfg
   = BlogCfg
   { bcfg_db   :: T.Text
   , bcfg_port :: Int
   , bcfg_name :: T.Text
   , bcfg_desc :: T.Text
}

type SessionVal = Maybe SessionId
type CommentSystem ctx = SpockCtxM ctx SqlBackend SessionVal B.ByteString ()
type CommentAction ctx a = SpockActionCtx ctx SqlBackend SessionVal B.ByteString a

commentSystem :: String -> CommentSystem ()
commentSystem dir =
  prehook baseHook $ do
    get "json" $ do
      allPosts <- fmap (map (entityVal)) . runSQL $ selectList [] [Desc CommentDate]
      json allPosts
    get ("static" <//> var) $ \fileName -> do
      file =<< (decodeUtf8 . defaultMimeLookup . T.pack) $ dir <> fileName
    post root $ void $ do
      now <- liftIO $ getCurrentTime
      comment@(Comment _ _ _ _ Nothing Nothing) <- jsonBody'
      runSQL $ insert comment

baseHook :: CommentAction () (HVect '[])
baseHook = return HNil
