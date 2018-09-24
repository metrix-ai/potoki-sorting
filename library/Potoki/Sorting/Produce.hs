module Potoki.Sorting.Produce
where

import Potoki.Sorting.Prelude
import qualified Acquire.Acquire as M
import qualified Potoki.Core.Produce as Produce
import qualified Potoki.Cereal.Produce as CerealProduce
import System.Directory

readOrderedFromFiles :: (Ord a, Serialize a) => [FilePath] -> Produce.Produce (Either IOException (Either Text a))
readOrderedFromFiles [] = mempty
readOrderedFromFiles paths = 
    foldl' (flip $ Produce.mergeOrdering ordBy . CerealProduce.fileDecoded) emptyProduce paths  
      where
        ordBy (Left _) _ = True
        ordBy (Right (Left _)) _ = True
        ordBy _ (Left _) = False
        ordBy _ (Right (Left _)) = False
        ordBy (Right (Right a)) (Right (Right b)) = a <= b

        dirPath = dropWhileEnd (/= '/') $ head paths
        emptyProduce = Produce.Produce $ M.Acquire $ return $ (empty, removePathForcibly dirPath)  

readOrderedFromFilesExplicitly :: (Serialize a) => (a -> a -> Ordering) -> [FilePath] -> Produce.Produce (Either IOException (Either Text a))
readOrderedFromFilesExplicitly _ [] = mempty
readOrderedFromFilesExplicitly f paths = 
    foldl' (flip $ Produce.mergeOrdering ordBy . CerealProduce.fileDecoded) emptyProduce paths  
      where
        ordBy (Left _) _ = True
        ordBy (Right (Left _)) _ = True
        ordBy _ (Left _) = False
        ordBy _ (Right (Left _)) = False
        ordBy (Right (Right a)) (Right (Right b))
          | ord == LT = True
          | ord == EQ = True
          | otherwise = False
          where
            ord = f a b

        dirPath = dropWhileEnd (/= '/') $ head paths
        emptyProduce = Produce.Produce $ M.Acquire $ return $ (empty, removePathForcibly dirPath)  