These are unique, sorted lists of characters for the first 4 HSK levels.

Generated with
```
import Data.List
a <- readFile "file-containing-hsk-word-list"
let b = concat $ fmap (words . (intersperse ' '))(words a)
let c = nub . sort $ b
writeFile "newFile.txt" (concat b)
```

Note: there is overlap between the files. But there are no repeated characters within a single file.
