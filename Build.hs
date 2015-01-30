import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import System.Directory
import Data.List
import Control.Monad

resolve :: FilePath -> FilePath -> FilePath
resolve f home | "_root" `isPrefixOf` f = '/' : dropDirectory1 f
               | ('_':g) <- f           = home </> '.' : g
               | otherwise              = error $ "resolve: file path not supported (" ++ f ++ ")"

cmdPairDo :: String -> (([FilePath] -> Action ()) -> FilePath -> FilePath -> Action ()) -> Action ()
cmdPairDo act fn = do
    src <- getDirectoryFiles "" ["_*//*"]
    home <- liftIO getHomeDirectory
    let argPairs = [ (f, resolve f home) | f <- src ]
    forM_ argPairs $ \(src, tgt) -> fn (cmd act) src tgt

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=".shake/"} $ do
    phony "pull" $ cmdPairDo "cp -av" (\cp s t -> cp [t, s])
    phony "push" $ cmdPairDo "cp -av" (\cp s t -> cp [s, t])
    phony "diff" $ cmdPairDo "diff -u" (\diff s t -> diff [t, s])

