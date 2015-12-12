import System.IO (hSetBuffering, stdout, BufferMode(..))

import FOMObot.App (runApp)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    runApp
