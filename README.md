

import           Data.Aeson
import           Control.DeepSeq
import qualified Data.ByteString.Lazy as BSL
json78m <- BSL.readFile "../corpus/78mbs.json"
let !x = deepseq json78m json78m
