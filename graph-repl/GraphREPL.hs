module Main where

import           Graphs

import           Data.Char                   (isDigit)
import           GHC.Word                    (Word8)

import           Codec.Picture
import           Control.Monad.State
import           System.Console.Haskeline

import           Graphics.Rasterific
import           Graphics.Rasterific.Texture

------------------------------------------------------------
-- Customize me!
------------------------------------------------------------

------------------------------------------------------------
-- Ignore the man behind the curtain (or peek if you like)
------------------------------------------------------------

data GraphState = GS
  { graph     :: Maybe Graph
  , graphSrc  :: Maybe String
  , graphSize :: (Int,Int)
  }

type GraphM = StateT GraphState (InputT IO)

defaultSize :: (Int, Int)
defaultSize = (256, 256)

showSize :: (Int, Int) -> String
showSize (w,h) = show w ++ "x" ++ show h

graphSettings :: Settings IO
graphSettings = defaultSettings
  { historyFile = Just "graph_history.txt" }

graphREPL :: IO ()
graphREPL = do
  putStrLn description
  runInputT graphSettings . flip evalStateT (GS Nothing Nothing defaultSize) $ loop
  where
    loop :: GraphM ()
    loop = do
      minput <- lift $ getInputLine "> "
      case minput of
        Nothing      -> return ()
        Just s       -> do
          let (cmd:rest) = words s
          shouldLoop <- case cmd of
            ":q"    -> return False
            ":quit" -> return False
            ":help" -> (lift $ outputStrLn helpMsg) >> return True
            ":size" -> sizeCmd rest                 >> return True
            ":save" -> saveCmd rest                 >> return True
            _       -> eval s                       >> return True
          when shouldLoop loop

sizeCmd :: [String] -> GraphM ()
sizeCmd [] = do
  curSz <- gets graphSize
  lift $ outputStrLn $ "The current size is " ++ showSize curSz ++ "."
sizeCmd (w:h:_)
  = case all isDigit w && all isDigit h of
      False -> lift $ outputStrLn "Please provide an integer width and height."
      True  -> do
        let sz = (read w, read h)
        modify (\qs -> qs { graphSize = sz })
        lift $ outputStrLn $ "Size for :saved images is now " ++ showSize sz


saveCmd []     = lift $ outputStrLn "Please provide a file name to save in."
saveCmd (fn:_) = do
  sz <- gets graphSize
  saveGraph True sz fn

saveGraph :: Bool -> (Int,Int) -> FilePath -> GraphM ()
saveGraph saveSrc gSize fn = do
  mq <- gets graph
  ms <- gets graphSrc
  case mq of
    Nothing -> lift $ outputStrLn "Nothing to save!"
    Just q  -> do
      let imgFn = fn ?<.> "png"
          srcFn = fn -<.> "txt"
      liftIO $ saveGraphRaw gSize imgFn q
      lift $ outputStrLn $ showSize gSize ++ " -> " ++ imgFn
      when saveSrc $ do
        liftIO $ maybe (return ()) (writeFile srcFn) ms
        lift   $ maybe (return ()) (\_ -> outputStrLn $ "source -> " ++ srcFn) ms

base ?<.> ext = if '.' `elem` base then base else base ++ "." ++ ext
base -<.> ext = takeWhile (/= '.') base ++ "." ++ ext

saveGraphRaw :: (Int,Int) -> FilePath -> Graph -> IO ()
saveGraphRaw (w,h) fn g = do
  let white = PixelRGBA8 255 255 255 255
      img = renderDrawing w h white g
  writePng fn img

eval :: String -> GraphM ()
eval s = case evalGraph s of
  Left  err  -> lift $ outputStrLn err
  Right qfun -> do
    modify (\qs -> qs { graph    = Just qfun
                      , graphSrc = Just s
                      }
           )
    saveGraph False defaultSize "graph.png"

main :: IO ()
main = graphREPL
