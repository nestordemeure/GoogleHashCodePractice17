module GHC.Potential 

open ExtCore.Collections
open ExtCore.IO
open GHC.Extensions
open GHC.Domain

let potential (pizza:Pizza) minIngr maxCells =
   MutableSet.empty