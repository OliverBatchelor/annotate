import qualified Client.Main as M

import Language.Javascript.JSaddle.WebKitGTK

main :: IO ()
main = run $ M.main "localhost:3000"
  
