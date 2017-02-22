
module GHC.Potential

open ExtCore.Collections
open ExtCore.IO
open GHC.Extensions
open GHC.Domain

let potential (pizza:Pizza) minIngr maxCells =
   MutableSet.empty

let is_legal_slice (slice:Slice) (minIngr:int) (maxCells:int) =
