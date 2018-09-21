module Potoki.Sorting.Transform
(
  sort,
)
where

import Potoki.Sorting.Prelude hiding (take, takeWhile, filter, drop, sort, index) 
import qualified Acquire.Acquire as M
import qualified Potoki.Core.Fetch as A
import qualified Potoki.Core.IO as IO
import qualified Potoki.Core.Consume as Consume
import qualified Potoki.Core.Produce as Produce
import qualified Potoki.Core.Transform as Transform
import qualified Potoki.Sorting.Produce as SortProduce
import qualified Potoki.Sorting.Consume as SortConsume

consumeAndProduce :: Consume.Consume input [Either a1 a]
     -> ([a] -> Produce.Produce (Either a1 b))
     -> Transform.Transform input (Either a1 b)
consumeAndProduce (Consume.Consume consume) produce =
  Transform.Transform $ \ (A.Fetch fetchIO) -> M.Acquire $ do    
    either <- consume $ (A.Fetch fetchIO)
    case sequence either of
      Left error -> return (A.Fetch $ return $ Just $ Left $ error, return ())
      Right paths -> do
        let (Produce.Produce fetchAndFinishAcquire) = produce paths
            (M.Acquire fetchAndFinishIO) = fetchAndFinishAcquire
        fetchAndFinishIO

sort :: (Ord a, Serialize a) => Int -> FilePath -> Transform.Transform a (Either IOException (Either Text a))
sort length dirPath = consumeAndProduce (SortConsume.sortAndSerializeConsume length dirPath) SortProduce.readOrderedFromFiles