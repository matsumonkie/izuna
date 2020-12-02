module ModuleAst.AppSpec where

import qualified Control.Monad.IO.Class as IO
import           Data.Function          ((&))
import qualified Data.Map               as Map
import qualified Data.Maybe             as Maybe
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Test.Hspec

import qualified ModuleAst.App          as App
import           ModuleAst.Model

spec :: Spec
spec =
  describe "parse hie files to ast" $ do
    it "0" $ do
      fileContent <- getModuleAst "./test/fixtures/hie/project0"

      fileContent `shouldBe` ["<pre>module Lib</pre>"
                             ,"<pre>    ( someFunc</pre>"
                             ,"<pre>    ) where</pre>"
                             ,"<pre></pre>"
                             ,"<pre>someFunc :: IO ()</pre>"
                             ,"<pre>someFunc = <span data-specialized-type='String -> IO ()'>putStrLn</span> <span data-specialized-type='String'>\"someFunc\"</span></pre>"
                             ]

    it "1" $ do
      fileContent <- getModuleAst "./test/fixtures/hie/project1"
      fileContent `shouldBe` ["<pre>module Lib</pre>"
                             ,"<pre>    ( someFunc</pre>"
                             ,"<pre>    ) where</pre>"
                             ,"<pre></pre>"
                             ,"<pre>import           Data.Foldable (foldl')</pre>"
                             ,"<pre></pre>"
                             ,"<pre>someFunc :: IO ()</pre>"
                             ,"<pre>someFunc = <span data-specialized-type='String -> IO ()'>putStrLn</span> <span data-specialized-type='String'>\"someFunc\"</span></pre>"
                             ,"<pre></pre>"
                             ,"<pre>add :: Int -> Int -> Int</pre>"
                             ,"<pre>add <span data-specialized-type='Int'>x</span> <span data-specialized-type='Int'>y</span> =</pre>"
                             ,"<pre>  <span data-specialized-type='Int'>x</span> <span data-specialized-type='Int -> Int -> Int'>+</span> <span data-specialized-type='Int'>y</span></pre>"
                             ,"<pre></pre>"
                             ,"<pre>sum :: [Int] -> Int</pre>"
                             ,"<pre>sum <span data-specialized-type='[Int]'>xs</span> =</pre>"
                             ,"<pre>  <span data-specialized-type='(Int -> Int -> Int) -> Int -> [Int] -> Int'>foldl'</span> <span data-specialized-type='Int -> Int -> Int'>add</span> <span data-specialized-type='Int'>0</span> <span data-specialized-type='[Int]'>xs</span></pre>"
                             ,"<pre></pre>"
                             ,"<pre>foo :: String</pre>"
                             ,"<pre>foo =</pre>"
                             ,"<pre>  let x = <span data-specialized-type='Integer'>42</span></pre>"
                             ,"<pre>  in <span data-specialized-type='Integer -> String'>show</span> <span data-specialized-type='Integer'>x</span></pre>"
                             ]

getModuleAst :: FilePath -> IO [Text]
getModuleAst filePath = do
  mapFileToModule <- IO.liftIO $ App.getModulesAst [filePath]
  mapFileToModule &
    Map.elems &
    Maybe.listToMaybe &
    Maybe.fromMaybe emptyModuleInfo &
    _minfo_fileContent &
    return
  where
    emptyModuleInfo :: ModuleInfo
    emptyModuleInfo = ModuleInfo
      { _minfo_asts = []
      , _minfo_fileContent = []
      }
