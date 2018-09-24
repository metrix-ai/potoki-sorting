# potoki-sorting

### Some features of implementation:
* Main function:
```haskell 
sort :: (Ord a, Serialize a) => Int -> FilePath -> Transform.Transform a (Either IOException (Either Text a))
```
* Fisrt parameter - the length of a piece of data is equal to the ratio of the total size of all data to the number of pieces to which the data is divided.
* Second parameter - directory for temporary files. 
  * This directory will be created if it does not already exist and will be completely **DELETED** when the algorithm finishes.
  * This implementation can be changed.
