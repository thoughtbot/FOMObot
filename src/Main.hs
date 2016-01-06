import System.IO (hSetBuffering, stdout, BufferMode(..))

import FOMObot.App (initApp)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    initApp
