import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import System.Directory
import Data.List
import Control.Monad

resolve :: FilePath -> FilePath -> FilePath
resolve f _ | "_root" `isPrefixOf` f = '/' : dropDirectory1 f
resolve ('_':f) home = home </> '.' : f
resolve f _ = error $ "resolve: file path not supported (" ++ f ++ ")"

cmdPairDo :: String -> (([FilePath] -> Action ()) -> FilePath -> FilePath -> Action ()) -> Action ()
cmdPairDo act fn = do
    src <- getDirectoryFiles "" ["_*//*"]
    home <- liftIO getHomeDirectory
    let argPairs = [ (f, resolve f home) | f <- src ]
    forM_ argPairs $ \(src, tgt) -> fn (cmd act) src tgt

main :: IO ()
main = do
    shakeArgs shakeOptions{shakeFiles=".shake/"} $ do
        phony "pull" $ cmdPairDo "cp" (\cp s t -> cp [t, s])
        phony "push" $ cmdPairDo "cp" (\cp s t -> cp [s, t])
        phony "diff" $ cmdPairDo "diff -u" (\diff s t -> diff [t, s])