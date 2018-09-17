module Potoki.Sorting.Transform
where

import Potoki.Sorting.Prelude hiding (take, takeWhile, filter, drop, sort, index) 
import qualified Acquire.Acquire as M
import qualified Potoki.Core.IO as IO
import qualified Potoki.Core.Produce as Produce
import qualified Potoki.Core.Transform as Transform
import qualified Potoki.Core.Consume as Consume
import qualified Potoki.Cereal.Consume as CerealConsume
import qualified Potoki.Cereal.Produce as CerealProduce
import qualified Data.Vector.Algorithms.Intro as Algo
import qualified Potoki.Core.Fetch as A
import qualified Data.Vector.Generic as GenericVector
import Control.Monad.Primitive
import System.Directory

sortAndSerializeVector :: (Ord a, Serialize a) => Transform.Transform (FilePath {-^ Path to the file to serialize to -}, Vector a) (Either IOException ())
sortAndSerializeVector =
  Transform.Transform $ \ (A.Fetch fetchVectorIO) -> M.Acquire $ do
    return $ (, return ()) $ A.Fetch $ do
      result <- fetchVectorIO
      case result of
        Nothing -> return Nothing
        Just (path, vector) -> do
          (mutableVector :: GenericVector.Mutable Vector (PrimState IO) a ) <- GenericVector.unsafeThaw vector
          Algo.sort mutableVector
          orderedVector <- ((GenericVector.unsafeFreeze mutableVector) :: IO (Vector a))
          (return . Just) =<< (IO.produceAndConsume (Produce.vector orderedVector) (CerealConsume.encodeToFile path))

sortAndSerialize :: (Ord a, Serialize a) => Int -> FilePath -> Transform.Transform a (Either IOException FilePath)
sortAndSerialize length dirPath = proc a -> do
  vectorOfA <- Transform.batch length -< a
  filePath <- filePaths dirPath -< ()
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

---------------------------------------------------------------------------------

sortAndSerializeConsume :: (Ord a, Serialize a) => Int -> FilePath -> Consume.Consume a [Either IOException FilePath]
sortAndSerializeConsume length dirPath =
  Consume.transform (sortAndSerialize length dirPath) $
  Consume.list

--------------------------------------------------------------------------------

readOrderedFromFiles :: (Ord a, Serialize a) => [FilePath] -> Produce.Produce (Either IOException (Either Text a))
readOrderedFromFiles paths = 
    foldr' (Produce.mergeOrdering ordBy . CerealProduce.fileDecoded) mempty paths  
      where
        ordBy (Left _) _ = True
        ordBy (Right (Left _)) _ = True
        ordBy _ (Left _) = False
        ordBy _ (Right (Left _)) = False
        ordBy (Right (Right a)) (Right (Right b)) = a <= b

        dirPath = dropWhileEnd (/= '/') $ head paths
          
