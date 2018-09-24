module Potoki.Sorting.Consume
where

import Potoki.Sorting.Prelude 
import qualified Potoki.Core.Consume as Consume
import qualified Potoki.Sorting.Consume.Transform as SortTransform

sortAndSerializeConsume :: (Ord a, Serialize a) => Int -> FilePath -> Consume.Consume a [Either IOException FilePath]
sortAndSerializeConsume length dirPath =
  Consume.transform (SortTransform.sortAndSerialize length dirPath) $
  Consume.list

sortAndSerializeConsumeExplicity :: (Serialize a) => (a -> a -> Ordering) -> Int -> FilePath -> Consume.Consume a [Either IOException FilePath]
sortAndSerializeConsumeExplicity ord length dirPath =
  Consume.transform (SortTransform.sortAndSerializeExplicity ord length dirPath) $
  Consume.list