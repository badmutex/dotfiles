#!/usr/bin/runhaskell

import Control.Applicative     ((<$>))
import Control.Concurrent      (threadDelay)
import Data.Maybe              (fromJust)
import System.Directory        (getDirectoryContents)
import System.Exit             (ExitCode)
import System.Posix.Files      (fileExist, touchFile, removeLink)
import System.FilePath         ((</>), (<.>), takeExtension)
import System.Process          (createProcess,shell,ProcessHandle,waitForProcess)
import System.Random           (getStdRandom,randomR)
import System.Time
import Text.Printf             (printf)
import qualified Data.Map as M (fromList,lookup,keys,Map)

data Viewer       = Viewer FilePath                       deriving (Read, Show)
data Exec         = Exec String                           deriving (Read, Show)
data Parameters   = Params String                         deriving (Read, Show)
data Flag         = Flag String                           deriving (Read, Show)
data Args         = Args [String]                         deriving (Read, Show)
data Time         = Seconds Int | Minutes Int | Hours Int deriving (Read, Show)

type Cache        = M.Map Int FilePath
newtype Directory = Directory FilePath                    deriving (Read, Show)
newtype Suffix    = Suffix String                         deriving (Eq,Show)
newtype ImageFile = ImageFile FilePath

data Config = Config {
      program    :: Viewer
    , images     :: Directory
    , shiftTime  :: Time
    } deriving (Read, Show)

defaultConfig = Config {
                  program    = Viewer "/usr/bin/feh"
                , images     = Directory "/home/badi/Backgrounds"
                , shiftTime  = Seconds 10
                }

defaultConfigDir = ".xmonad"
defaultConfigFileName = "random-background.conf"
defaultConfigFile = "/home/badi" </> defaultConfigDir </> defaultConfigFileName
defaultSuffixes = map (\s -> Suffix (""<.>s)) ["png","jpg","jpeg"]


mkExec :: Viewer -> ImageFile -> Exec
mkExec (Viewer exe) (ImageFile f) = Exec $ printf "%s --bg-scale '%s'" exe f

buildExec :: Config -> ImageFile -> Exec
buildExec c  = mkExec (program c)

runShell :: (ProcessHandle -> IO a) -> Exec -> IO a
runShell handler (Exec exe) = do (_,_,_,h) <- createProcess . shell $ exe
                                 handler h

wait = waitForProcess

changeBackground :: Exec -> IO ExitCode
changeBackground = runShell wait

readConfigFile :: FilePath -> IO Config
readConfigFile filename = read <$> readFile filename




sleep :: Time -> IO ()
sleep time = let secondsToMicro = (*) 1000000
                 td = threadDelay
             in td . secondsToMicro
                    $ case time of
                        Seconds s -> s
                        Minutes m -> m * 60
                        Hours   h -> h * 60^2


chooseFiles :: [Suffix] -> [FilePath] -> M.Map Int FilePath
chooseFiles ss fs = M.fromList . zip [0..] $ filter choose fs
    where choose f = let suffix = Suffix (takeExtension f)
                     in suffix `elem` ss

buildCache :: [Suffix] -> Directory -> IO Cache
buildCache ss (Directory d) = do
  contents <- getDirectoryContents d
  return $ chooseFiles ss contents


chooseImage :: Cache -> Int -> ImageFile
chooseImage cache ix = ImageFile . fromJust $ M.lookup ix cache


loop confile cache csize = do
  conf <- readConfigFile confile
  ix   <- getStdRandom (randomR (0,csize))
  let img                        = images conf /> chooseImage cache ix
      exe                        = buildExec conf img
      Directory d /> ImageFile f = ImageFile (d </> f)
  time <- getClockTime
  changeBackground exe
  putStrLn $ printf "[%s] %s" (show time) (show exe)
  sleep (shiftTime conf)
  loop confile cache csize


lock :: IO () -> IO ()
lock io = do
  exists <- fileExist _lockfile
  if exists
    then do putStrLn "Already running!"
            return ()
    else do writeFile _lockfile ""
            io
            removeLink _lockfile


mainloop = do
  cache <- buildCache defaultSuffixes (images defaultConfig)
  let size = length $ M.keys cache
  loop defaultConfigFile cache size


main = mainloop


-- ---------------------------------------- --
_lockfile = "/home/badi/.xmonad/random-background.lock"
-- ---------------------------------------- --