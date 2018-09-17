module Potoki.Sorting.Transform
where

import Potoki.Sorting.Prelude hiding (take, takeWhile, filter, drop) 
import qualified Acquire.Acquire as M
import qualified Potoki.Core.IO as IO
import qualified Potoki.Core.Produce as Produce
import qualified Potoki.Core.Transform as Transform
import qualified Potoki.Cereal.Consume as CerealConsume
import qualified Data.Vector.Algorithms.Intro as Algo
import qualified Potoki.Core.Fetch as A
import qualified Data.Vector.Generic as GenericVector
import Control.Monad.Primitive

writeBatchToFile :: (Ord a, Serialize a) => FilePath -> Transform.Transform (Vector a) (Either IOException ())
writeBatchToFile dir =
  Transform.Transform $ \ (A.Fetch fetchVectorIO) -> M.Acquire $ do
    countRef <- newIORef 0
    return $ (, return ()) $ A.Fetch $ do
      result <- fetchVectorIO
      case result of
        Nothing -> return Nothing
        Just vector -> do
          number <- readIORef countRef
          let path = dir ++ "/" ++ (show number)
          (mutableVector :: GenericVector.Mutable Vector (PrimState IO) a ) <- GenericVector.unsafeThaw vector
          Algo.sort mutableVector
          orderedVector <- ((GenericVector.unsafeFreeze mutableVector) :: IO (Vector a))
          modifyIORef' countRef succ
          (return . Just) =<< (IO.produceAndConsume (Produce.vector vector) (CerealConsume.encodeToFile path))
