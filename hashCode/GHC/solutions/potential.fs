module GHC.Potential 

open GHC.Extensions
open GHC.Domain

let potential (pizza:Pizza) minIngr maxCells =
   MutableSet.empty