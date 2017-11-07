#! /usr/bin/env runghc

{-# LANGUAGE OverloadedStrings #-}

module Syncer (main) where

import Data.Monoid       ((<>))
import System.FilePath   (takeBaseName, takeDirectory)
import System.Posix.Env  (getEnv)
import System.Posix.Temp (mkdtemp)
import System.Process    (callProcess, readProcess)

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

createJobs :: Env -> [RsyncJob]
createJobs env =
  let home = _home env
      host = _host env
  in [ RsyncJob { _hosts         = ["thaumas"]
                , _sources       = [home <> "/tmp"]
                , _target        = "backup:Dropbox/bup/machines/" <> host
                , _rsyncFlags    = ["-a"]
                , _shouldTarball = False
                , _tarFlags      = []
                }
     , RsyncJob { _hosts         = ["nereus"]
                , _sources       = ["/srv/git"]
                , _target        = "backup:Dropbox/bup/machines/" <> host
                , _rsyncFlags    = ["-a"]
                , _shouldTarball = True
                , _tarFlags      = []
                }
     , RsyncJob { _hosts         = ["nereus", "thaumas"]
                , _sources       = [home <> "/.emacs.d/elpa"]
                , _target        = "backup:Dropbox/bup/machines/" <> host
                , _rsyncFlags    = ["-rlptD", "--remove-source-files"]
                , _shouldTarball = True
                , _tarFlags      = ["-p", "--exclude=*.elc", "--exclude=S.gpg-agent*"]
                }
     ]

getHome :: IO FilePath
getHome = maybe (error "Could not get HOME") return =<< getEnv "HOME"

getHostname :: IO String
getHostname = stripNewlines <$> readProcess "hostname" ["-s"] []
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

process :: RsyncJob -> FilePath -> IO ()
process job source =
  if (_shouldTarball job)
  then do
    dir <- mkdtemp "/tmp/syncer-"
    let tarballTarget = concat [dir, "/", takeBaseName source, ".tar.gz"]
    tar (_tarFlags job) tarballTarget source
    putStrLn ("Compressed " ++ source ++ " to " ++ tarballTarget)
    rsync (_rsyncFlags job) (_target job) tarballTarget
    putStrLn ("Copied " ++ tarballTarget ++ " to " ++ (_target job))
  else do
    rsync (_rsyncFlags job) (_target job) source
    putStrLn ("Copied " ++ source ++ " to " ++ (_target job))

runOne :: RsyncJob -> IO ()
runOne j = mapM_ f (_sources j)
  where
    f s = process j s

runMany :: (Env -> [RsyncJob]) -> IO ()
runMany k = do
  home     <- getHome
  thisHost <- getHostname
  let jobs   = k Env{_home = home, _host = thisHost}
      myJobs = filter (elem thisHost . _hosts) jobs
  mapM_ runOne myJobs

main :: IO ()
main = runMany createJobs
