#! /usr/bin/env nix-shell
#! nix-shell -i runghc -p "haskellPackages.ghcWithPackages (pkgs: [pkgs.transformers])"

{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer.Strict
import Data.Monoid
import System.Exit
import System.FilePath
import System.Posix.Env
import System.Posix.Files
import System.Posix.Temp
import System.Process

data Env = Env
  { _home :: FilePath
  , _host :: String
  }

data RsyncJob = RsyncJob
  { _hosts         :: [String]
  , _sources       :: [FilePath]
  , _target        :: FilePath
  , _rsyncFlags    :: [String]
  , _shouldTarball :: Bool
  , _tarFlags      :: [String]
  }

jobs :: Env -> [RsyncJob]
jobs env =
  let home = _home env
      host = _host env
  in [ RsyncJob { _hosts         = ["thaumas"]
                , _sources       = [ home <> "/tmp" ]
                , _target        = "backup:Dropbox/bup/machines/" <> host
                , _rsyncFlags    = ["-a"]
                , _shouldTarball = False
                , _tarFlags      = []
                }
     , RsyncJob { _hosts         = ["nereus"]
                , _sources       = [ "/srv/git" ]
                , _target        = "backup:Dropbox/bup/machines/" <> host
                , _rsyncFlags    = ["-a"]
                , _shouldTarball = True
                , _tarFlags      = []
                }
     , RsyncJob { _hosts         = ["nereus", "thaumas"]
                , _sources       = [ home <> "/.emacs.d/elpa" ]
                , _target        = "backup:Dropbox/bup/machines/" <> host
                , _rsyncFlags    = ["-rlptD", "--remove-source-files"]
                , _shouldTarball = True
                , _tarFlags      = ["-p", "--exclude=*.elc"]
                }
     ]

getHome :: IO FilePath
getHome = maybe (error "Could not get HOME") return =<< getEnv "HOME"

hostname :: IO String
hostname = stripNewlines <$> readProcess "hostname" ["-s"] []
  where
    stripNewlines :: String -> String
    stripNewlines = filter (/= '\n')

rsync :: [String] -> FilePath -> FilePath -> IO ()
rsync flags target source =
  callProcess "rsync" (flags ++ [source, target])

tar :: [String] -> FilePath -> FilePath -> IO ()
tar flags target source =
  callProcess "tar" (flags ++
                     ["-cz"] ++
                     ["-f" ++ target] ++
                     ["-C" ++ takeDirectory source] ++
                     [takeBaseName source])

process :: RsyncJob -> FilePath -> WriterT [String] IO ()
process job source =
  if (_shouldTarball job)
  then do
    dir <- liftIO $ mkdtemp "/tmp/syncer-"
    let tarballTarget = concat [dir, "/", takeBaseName source, ".tar.gz"]
    liftIO $ tar (_tarFlags job) tarballTarget source
    tell ["Compressed " ++ source ++ " to " ++ tarballTarget]
    liftIO $ rsync (_rsyncFlags job) (_target job) tarballTarget
    tell ["Copied " ++ tarballTarget]
  else do
    liftIO $ rsync (_rsyncFlags job) (_target job) source
    tell ["Copied " ++ source]

runOne :: RsyncJob -> IO ()
runOne j = mapM_ f (_sources j)
  where
    f s = execWriterT (process j s) >>= putStr . unlines

runMany :: (Env -> [RsyncJob]) -> IO ()
runMany jobs = do
  home     <- getHome
  thisHost <- hostname
  let js   = jobs Env{ _home = home, _host = thisHost}
      myJs = filter (elem thisHost . _hosts) js
  mapM_ runOne myJs

main :: IO ()
main = runMany jobs
