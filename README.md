## Running

Run in `sbt` shell
```
runMain test.SimpleSearch <path to directory>
```

## Overview

The algorithm calculates a hash of each word and stores the hashes in a set (per file).
The score determines how many % of the queried words are found in each file.

## Future improvements

Weighted score - some short English words (I, am, to, be, etc.) are less significant
than others and often irrelevant for text searching.
Such words could have smaller impact on the overall score if we would use
weighted score calculation.
