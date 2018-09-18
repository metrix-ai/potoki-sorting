module Potoki.Sorting.Consume.Transform
where

import Potoki.Sorting.Prelude hiding (take, takeWhile, filter, drop, sort, index) 
import qualified Acquire.Acquire as M
import qualified Potoki.Core.IO as IO
import qualified Potoki.Core.Produce as Produce
import qualified Potoki.Core.Transform as Transform
import qualified Potoki.Core.Consume as Consume
import qualified Potoki.Cereal.Consume as CerealConsume
import qualified Data.Vector.Algorithms.Intro as Algo
import qualified Potoki.Core.Fetch as A
import qualified Data.Vector.Generic as GenericVector

sortAndSerializeVector :: (Ord a, Serialize a) => Transform.Transform (FilePath {-^ Path to the file to serialize to -}, Vector a) (Either IOException ())
sortAndSerializeVector =
  Transform.Transform $ \ (A.Fetch fetchVectorIO) -> M.Acquire $ do
    return $ (, return ()) $ A.Fetch $ do
      result <- fetchVectorIO
      case result of
        Nothing -> return Nothing
        Just (path, vector) -> do
          traceM (show $ GenericVector.length vector)
          (mutableVector :: GenericVector.Mutable Vector RealWorld a) <- GenericVector.unsafeThaw vector
          Algo.sort mutableVector
          orderedVector <- ((GenericVector.unsafeFreeze mutableVector) :: IO (Vector a))
          (return . Just) =<< (IO.produceAndConsume (Produce.vector orderedVector) (CerealConsume.encodeToFile path))

sortAndSerialize :: (Ord a, Serialize a) => Int -> FilePath -> Transform.Transform a (Either IOException FilePath)
sortAndSerialize length dirPath = proc a -> do
  vectorOfA <- Transform.batch length -< a
  filePath <- filePaths dirPath -< trace (show (GenericVector.length vectorOfA)) ()
  sortingResult <- sortAndSerializeVector -< (filePath, vectorOfA)
  returnA -< fmap (const filePath) sortingResult

index :: Transform.Transform a Int
index = 
  Transform.Transform $ \ (A.Fetch _) -> M.Acquire $ do
    indexRef <- newIORef 0
    return $ (, return ()) $ A.Fetch $ do
      number <- readIORef indexRef
      writeIORef indexRef (succ number)
      return $ Just number

filePaths :: FilePath -> Transform.Transform a FilePath
filePaths dirPath = rmap indexToPath index where
  indexToPath index = dirPath <> "/" <> show index
