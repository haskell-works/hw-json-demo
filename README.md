# hw-json-demo

[![CircleCI](https://circleci.com/gh/haskell-works/hw-json-demo.svg?style=svg)](https://circleci.com/gh/haskell-works/hw-json-demo)

```haskell
import           Data.Aeson
import           Control.DeepSeq
import qualified Data.ByteString.Lazy as BSL
json78m <- BSL.readFile "../corpus/78mbs.json"
let !x = deepseq json78m json78m
```
