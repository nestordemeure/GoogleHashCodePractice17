
module GHC.Potential

open ExtCore.Collections
open ExtCore.IO
open GHC.Extensions
open GHC.Domain

let potential (pizza:Pizza) minIngr maxCells =
   MutableSet.empty

let isLegalSlice (pizza:Pizza) (slice:Slice) (minIngr:int) (maxCells:int) =
   let mutable nbMushroom = 0
   let mutable nbTomato = 0
   for x=slice.left to slice.right do
      for y=slice.bottom to slice.top do
         match pizza.[x, y] with
         | M -> nbMushroom <- nbMushroom+1
         | T -> nbTomato <- nbTomato+1
   (nbMushroom > minIngr) && (nbTomato > minIngr) && (nbTomato+nbMushroom < maxCells)
