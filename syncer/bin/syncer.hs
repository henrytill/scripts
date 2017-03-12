#! /usr/bin/env runghc

import Control.Monad
import Data.Monoid
import System.Exit
import System.FilePath
import System.Posix.Env
import System.Posix.Files
import System.Posix.Temp
import System.Process

data RsyncJob = RsyncJob
  { hosts         :: [String]
  , sources       :: FilePath -> [FilePath]
  , target        :: String   -> FilePath
  , rsyncFlags    :: [String]
  , shouldTarball :: Bool
  , tarFlags      :: [String]
  }

jobs :: [RsyncJob]
jobs =
  [ RsyncJob { hosts         = ["thaumas"]
             , sources       = \home -> [ home <> "/tmp"
                                        , home <> "/doc"
                                        ]
             , target        = \host -> "backup:Dropbox/bup/machines/" <> host
             , rsyncFlags    = ["-a"]
             , shouldTarball = False
             , tarFlags      = []
             }
  , RsyncJob { hosts         = ["nereus"]
             , sources       = const [ "/srv/git" ]
             , target        = \host -> "backup:Dropbox/bup/machines/" <> host
             , rsyncFlags    = ["-a"]
             , shouldTarball = True
             , tarFlags      = []
             }
  , RsyncJob { hosts         = ["nereus", "thaumas"]
             , sources       = \home -> [ home <> "/.emacs.d/elpa" ]
             , target        = \host -> "backup:Dropbox/bup/machines/" <> host
             , rsyncFlags    = ["-rlptD", "--remove-source-files"]
             , shouldTarball = True
             , tarFlags      = ["-p", "--exclude=*.elc"]
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

process :: String -> RsyncJob -> FilePath -> IO ()
process host job source =
  if (shouldTarball job)
  then do
    dir <- mkdtemp "/tmp/syncer"
    let tarballTarget = concat [dir, "/", takeBaseName source, ".tar.gz"]
    putStrLn ("Compressing " ++ source ++ " to " ++ tarballTarget)
    tar (tarFlags job) tarballTarget source
    putStrLn ("Copying " ++ tarballTarget)
    rsync (rsyncFlags job) (target job host) tarballTarget
  else do
    putStrLn ("Copying " ++ source)
    rsync (rsyncFlags job) (target job host) source

runOne :: String -> RsyncJob -> IO ()
runOne host j = sources j <$> getHome >>= mapM_ (process host j)

runMany :: [RsyncJob] -> IO ()
runMany js = do
  thisHost <- hostname
  myJobs   <- pure $ filter (\job -> thisHost `elem` hosts job) js
  mapM_ (runOne thisHost) myJobs

main :: IO ()
main = runMany jobs
