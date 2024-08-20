import GHC.IO.Handle (hGetContents)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (hClose)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, waitForProcess)
import Test.Hspec

spec :: Spec
spec = do
  describe "daicker exec" $ do
    it "add" $
      unsafePerformIO (daicker ["eval", "-f", "test/e2e/asset/main.daic", "add", "1", "2"]) `shouldBe` Result ExitSuccess "3\n" ""

data Result = Result {exitCode :: ExitCode, stdout :: String, stderr :: String} deriving (Show, Eq)

daicker :: [String] -> IO Result
daicker args = do
  (_, Just stdout, Just stderr, ps) <-
    createProcess (proc "daicker" args) {std_out = CreatePipe, std_err = CreatePipe, delegate_ctlc = True, std_in = CreatePipe}
  stdout' <- hGetContents stdout
  stderr' <- hGetContents stderr
  exitCode <- waitForProcess ps
  pure $ Result exitCode stdout' stderr'

main :: IO ()
main = hspec spec
