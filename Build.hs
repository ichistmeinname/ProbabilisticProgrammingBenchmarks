import           Data.Char                  (toLower)
import           Data.List                  (intercalate)

import           System.Exit                (ExitCode (..))

import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath
import           Development.Shake.Util

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
      cmd_ "rm Curry/SecretSanta"

    phony "benchmarks" $ do
      cmd_ "mkdir -p" "html"
      need ["dependencies"]
--      need ["strings", "stringsFast", "bayes", "replicate", "curry-strings", "curry-strings-vs-fast", "curry-die"
--           , "webppl-strings", "webppl-die", "santa"]
      need ["santa"]

    (curryDir </> "*") %> \out -> do
      let file = takeBaseName out
      putNormal ("Save Curry executable ./" ++ file)
      cmd_ kics2 ":set v0" ":set dfs" ":set path" pflpDep ":l" (out <.> "curry") ":save :quit"

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
      let curryArgs = makeArgs Curry "ReplicateDie" "" [2,3,4,5,6,7,8,9,10]
          probArgs = makeArgs ProbLog (problog </> "replicateDie.py") "" [2,3,4,5]
          webArgs = makeArgs WebPPL (webppl </> "replicateDie.wppl") "" [2,3,4,5,6,7,8,9]
          args = curryArgs ++ probArgs ++ webArgs
      cmd_ benchExe args "--output html/ReplicateDie.html"

    phony "bayes" $ do
      need [curryDir </> "Bayes"]

      putNormal "Benchmark bayesian network"
      let args = collectArgs "Bayes" "" [1,2,3]
      cmd_ benchExe args "--output html/Bayes.html"

    phony "pakcs" $
      let benchPakcs (exeName,shellFile,additionalArg, inputs) = do
            putNormal ("Save Curry executable " ++ exeName)
            cmd_ "pakcs" ":set v0" ":set path" pflpDep ":l" ((curryDir </> exeName) <.> "curry") ":save :quit"

            putNormal ("Benchmark " ++ exeName)
            let argsC = map (\arg -> intercalate " " [shellFile, show arg, additionalArg]) inputs
            cmd_ benchExe argsC "--output" ("html" </> (exeName ++ "PAKCS") <.> "html")
      in mapM_ benchPakcs [("SecretSanta","./santa.sh", "wofailed", [2..8])
                         , ("ReplicateDie","./replicate.sh", "", [2..10])
                         , ("Bayes","./bayes.sh", "", [1..3])
                         , ("Strings","./strings.sh", "", [5,10,15,20,25,30])
                         , ("Strings","./strings.sh", "fast", [5,10,15,20,25,30])
                         , ("SecretSanta","./santa.sh", "", [2..8])
                         ]

    phony "santa" $ do

      putNormal "Benchmark secret santa"
      let argsP = makeArgs ProbLog (problog </> "secretSanta.py") "" [2..8]
          argsW = makeArgs WebPPL (webppl </> "secretSanta.wppl") "" [2..10]
      cmd_ benchExe (argsP ++ argsW) "--output html/SecretSantaExtras.html"

      putNormal "Benchmark secret santa with modified hat"
      let argsP = makeArgs ProbLog (problog </> "secretSanta.py") "wofailed" [2..8]
          argsW = makeArgs WebPPL (webppl </> "secretSanta.wppl") "wofailed" [2..10]
      cmd_ benchExe (argsP ++ argsW) "--output html/SecretSantaModifiedHatExtras.html"

    phony "stringsFast" $ do
      need [curryDir </> "Strings"]

      putNormal "Benchmark palindrome fast"
      let curryArgs = makeArgs Curry "Strings" "fast" [5,10,15,20,25]
          probArgs = makeArgs ProbLog (problog </> "stringsFast.py") "" [5,10,15,20,25]
          webArgs = makeArgs WebPPL (webppl </> "stringsFast.wppl") "" [5,10,15,20,25]
          argsPF = curryArgs ++ probArgs ++ webArgs
      cmd_ benchExe argsPF "--output html/StringsPalindromeFast.html"

    phony "webppl-strings" $ do
      let webArgs1 = makeArgs WebPPL (webppl </> "strings.wppl") "" [5,10,15,20,25]
          webArgs2 = makeArgs WebPPL (webppl </> "stringsFast.wppl") "" [5,10,15,20,25,30,35,40,45,50]
      cmd_ benchExe (webArgs1 ++ webArgs2) "--output html/WebPPLStrings.html"

    phony "webppl-die" $ do
      let webArgs = makeArgs WebPPL (webppl </> "replicateDie.wppl") "" [2,3,4,5,6,7,8,9]
      cmd_ benchExe webArgs "--output html/WebPPLDie.html"

    phony "webppl-santa" $ do
      let webArgs1 = makeArgs WebPPL (webppl </> "secretSanta.wppl") "wofailed" [2,3,4,5,6,7,8,9,10]
      let webArgs2 = makeArgs WebPPL (webppl </> "secretSanta.wppl") "" [2,3,4,5,6,7,8,9,10]
      cmd_ benchExe (webArgs1 ++ webArgs2) "--output html/WebPPLSanta.html"

    phony "curry-strings-vs-fast" $ do
      need [curryDir </> "Strings"]

      putNormal "Benchmark Curry naive strings vs fast strings"
      let curryArgs = makeArgs Curry "Strings" ""     [5,10,15,20,25,30,35]
          probArgs  = makeArgs ProbLog (problog </> "stringsFast.py") "" [5,10,15,20,25,30,35]
          webArgs   = makeArgs WebPPL (webppl </> "stringsFast.wppl") "" [5,10,15,20,25,30,35]
          argsPF    = curryArgs ++ probArgs ++ webArgs
      cmd_ benchExe argsPF "--output html/CurryStringsVsFast.html"

    phony "curry-strings" $ do
      need [curryDir </> "Strings"]

      putNormal "Benchmark Curry Strings"
      let curry     = makeArgs Curry "Strings" ""     [5,10,15,20,25,30]
          curryFast = makeArgs Curry "Strings" "fast" [5,10,15,20,25,30,35,40]
      cmd_ benchExe (curry ++ curryFast) "--output html/CurryStrings.html"

    phony "curry-die" $ do
      need [curryDir </> "ReplicateDie"]

      let curry = makeArgs Curry "ReplicateDie" "" [25,50,100,250,500,1000,2500]
      cmd_ benchExe curry "--output html/CurryDie.html"

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
