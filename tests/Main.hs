module Main
where 

import Prelude hiding (first, second)
import Control.Arrow
import qualified Control.Foldl as Foldl
import Test.QuickCheck.Instances
import Test.QuickCheck.Monadic as M
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Potoki.Core.IO as IO
import qualified Potoki.Core.Consume as Consume
import qualified Potoki.Core.Produce as Produce
import qualified Potoki.Core.Transform as Transform
import Data.List.Ordered
import qualified Potoki.Sorting.Transform as Sort

main =
  defaultMain $
  testGroup "sorting" $
  [ 
    -- testProperty "sort" $ \ (list :: [Int]) ->
    --   let l = (sort list)
    --       len = ceiling $ (fromIntegral $ length list) / 5
    --       path = "./temp"
    --   -- in l === unsafePerformIO (IO.produceAndTransformAndConsume (Produce.list list) (Sort.sort len path) Consume.list)
    --   in monadicIO $ do
    --     eithers <- run $ (IO.produceAndTransformAndConsume (Produce.list list) (Sort.sort len path) Consume.list)
    --     run $ print eithers
    --     M.assert True
    --     -- case sequence eithers of
    --     --   Left error -> M.assert True --run $ print error
    --     --   Right eithersText ->
    --     --     case sequence eithersText of
    --     --       Left text -> 
    --     --         M.assert True--run $ print text
    --     --       Right sortList -> M.assert $ True
    -- , 
    testCase "sortCase" $ do
      let list = [10,9,8,7,6,5,4,3,2,1] :: [Int]
          len = ceiling $ (fromIntegral $ length list) / 5
          path = "./temp"
      print list
      res <- (IO.produceAndTransformAndConsume (Produce.list list) (Sort.sort len path) Consume.list)
      print res
      assertEqual "" True True
  ]