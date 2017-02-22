
module GHC.Potential

open ExtCore.Collections
open ExtCore.IO
open GHC.Extensions
open GHC.Domain

let potential (pizza:Pizza) minIngr maxCells =
    let mutable sliceLength = minIngr*2
    while sliceLength>=1 do  
        MutableSet.add ({ left = 0 ; top = 0 ; right = 0; bottom = 0; score=0}) 
        sliceLength<-sliceLength/2
   MutableSet.empty

let isLegalSlice (pizza:Pizza) (slice:Slice) (minIngr:int) (maxCells:int) =
   let mutable nbMushroom = 0
   let mutable nbTomato = 0
   for x=slice.left to slice.right do
      for y=slice.bottom to slice.top do
         match pizza.[x, y] with
         | M -> nbMushroom <- nbMushroom+1
         | T -> nbTomato <- nbTomato+1
   (nbMushroom >= minIngr) && (nbTomato >= minIngr) && (nbTomato+nbMushroom <= maxCells)
