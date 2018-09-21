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
    testProperty "sort" $ \ (list :: [Int]) ->
      let ordList = (sort list)
          len = ceiling $ (fromIntegral $ length list) / 5
          path = "./temp"
      in monadicIO $ do
        eithers <- run $ (IO.produceAndTransformAndConsume (Produce.list list) (Sort.sort len path) Consume.list)
        if eithers == []
          then M.assert $ [] == list
          else                    
            case sequence eithers of
                Left error -> M.assert $ (show error) == ""
                Right eithersText ->
                  case sequence eithersText of
                    Left text -> M.assert $ text == ""
                    Right sortList -> M.assert $ ordList == sortList
    , 
    testCase "sortCase" $ do
      let list = [10,9,8,7,6,5,4,3,2,1] :: [Int]
          len = ceiling $ (fromIntegral $ length list) / 5
          path = "./temp"
          ordList = sort list
      print list
      eithers <- (IO.produceAndTransformAndConsume (Produce.list list) (Sort.sort len path) Consume.list)
      if eithers == []
        then assertEqual "" [] list
        else                    
          case sequence eithers of
              Left error -> assertEqual "" (show error) ""
              Right eithersText ->
                case sequence eithersText of
                  Left text -> assertEqual "" text ""
                  Right sortList -> assertEqual "" ordList sortList
  ]