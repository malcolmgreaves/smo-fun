

type Alpha = Double
type Target = Int
type Alphas = Seq[Alpha]

import fif.Data
import Data.ops._


val data: { D[(Alpha, Target)] forSome D[_] : Data } = ...

