import Data.List (intercalate)
import Data.Char (toLower)

import System.Exit (ExitCode(..))

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

data Tool = Curry | WebPPL | ProbLog

curryDir = "Curry"
webppl = "WebPPL"
problog = "ProbLog"
bench = "bench"
benchExe = ".cabal-sandbox/bin/bench"
npm = "npm"
pip = "pip"
kics2 = "kics2"
pflpDep = curryDir </> "pflp" </> "src"

toolPath Curry   exeName = "." </> curryDir </> exeName
toolPath ProbLog _       = "python"
toolPath WebPPL  _       = "." </> webppl </> "node_modules" </> ".bin" </> "webppl"

makeArgs tool@Curry exeName arg1 args = map (\arg -> intercalate " " [toolPath tool exeName, arg1, show arg]) args
makeArgs tool       exeName arg1 args = map (\arg -> intercalate " " [toolPath tool exeName, exeName, arg1, show arg]) args

collectArgs exeName arg1 args =
  let exeNameLower = map toLower exeName
      curryArgs = makeArgs Curry exeName arg1 args
      probArgs = makeArgs ProbLog (problog </> exeNameLower <.> "py") arg1 args
      webArgs = makeArgs WebPPL (webppl </> exeNameLower <.> "wppl") arg1 args
  in curryArgs ++ probArgs ++ webArgs
  
main :: IO ()
main = shakeArgs shakeOptions $ do
    want ["benchmarks"]

    phony "clean" $ do
      putNormal "Remove outputs and executables"
      cmd_ "rm -rf html"
      cmd_ "rm Curry/Strings"
      cmd_ "rm Curry/ReplicateDie"
      cmd_ "rm Curry/Bayes"

    phony "benchmarks" $ do
      cmd_ "mkdir -p" "html"
      need ["dependencies"]
      need ["strings", "stringsFast", "bayes", "replicate"]

    (curryDir </> "*") %> \out -> do
      let file = takeBaseName out
      putNormal ("Save Curry executable ./" ++ file)
      cmd_ kics2 ":set v0" ":set path" pflpDep ":l" (out <.> "curry") ":save :quit"
      
    phony "strings" $ do
      need [curryDir </> "Strings"]

      putNormal "Benchmark palindrome"
      let argsP = collectArgs "Strings" "" [5,6,7,8,9,10]
      cmd_ benchExe argsP "--output html/StringsPalindrome.html"

      putNormal "Benchmark consecutiveBs"
      let argsB = collectArgs "Strings" "bs" [5,6,7,8,9,10]
      cmd_ benchExe argsB "--output html/StringsBs.html"

    phony "stringsFast" $ do
      need [curryDir </> "Strings"]

      putNormal "Benchmark palindrome fast"
      let curryArgs = makeArgs Curry "Strings" "fast" [5,10,15,20,25]
          probArgs = makeArgs ProbLog (problog </> "stringsFast.py") "" [5,10,15,20,25]
          webArgs = makeArgs WebPPL (webppl </> "stringsFast.wppl") "" [5,10,15,20,25]
          argsPF = curryArgs ++ probArgs ++ webArgs
      cmd_ benchExe argsPF "--output html/StringsPalindromeFast.html"

    phony "replicate" $ do
      need [curryDir </> "ReplicateDie"]

      putNormal "Benchmark replicated die"
      let curryArgs = makeArgs Curry "ReplicateDie" "" [2,3,4,5,10,15,25,50,100,200]
          probArgs = makeArgs ProbLog (problog </> "replicateDie.py") "" [2,3,4,5]
          webArgs = makeArgs WebPPL (webppl </> "replicateDie.wppl") "" [2,3,4,5,6,7]
          args = curryArgs ++ probArgs ++ webArgs
      cmd_ benchExe args "--output html/ReplicateDie.html"
    
    phony "bayes" $ do
      need [curryDir </> "Bayes"]

      putNormal "Benchmark bayesian network"
      let args = collectArgs "Bayes" "" [""]
      cmd_ benchExe args "--output html/Bayes.html"

    phony "dependencies" $ do
      need [curryDir, webppl, problog, bench]

    let which arg = command [] "command" ["-v", arg]

    phony webppl $ do
      Exit out <- which "webppl"
      case out of
        ExitFailure _ -> do
          Exit out1 <- which (webppl </> "node_modules" </> ".bin" </> "webppl")
          case out1 of
            ExitFailure _ -> do
              Exit out2 <- which npm
              case out2 of
                ExitFailure _ -> putNormal ("Could not find executable " ++ npm ++ ", please install it first.")
                _ -> do
                  cmd_ npm "install" "--prefix" webppl "webppl"
            _ -> return ()
        _ -> return ()


    phony problog $ do
      Exit out1 <- which "problog"
      case out1 of
        ExitFailure _ -> do
          Exit out2 <- which pip
          case out2 of
            ExitFailure _ -> putNormal ("Could not find executable " ++ pip ++ ", please install it first.")
            _ -> do
              cmd_ pip "install" "problog" "-t" problog
        _ -> return ()

    phony curryDir $ do
      Exit out <- which kics2
      case out of
        ExitFailure _ -> putNormal ("Could not find executable " ++ kics2 ++ ", please install it first.")
        _ -> return ()

    phony bench $ do
      Exit out1 <- which benchExe
      case out1 of
        ExitFailure _ -> do
          Exit out2 <- which bench
          case out2 of
            ExitFailure _ -> do
              cmd_ "cabal sandbox init"
              cmd_ "cabal install" bench
            _ -> return ()
        _ -> return ()
