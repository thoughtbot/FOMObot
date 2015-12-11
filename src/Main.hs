import System.IO (hSetBuffering, stdout, stderr, BufferMode(..))

import FOMObot.App (runApp)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    runApp
