
module GHC.Potential

open ExtCore.Collections
open ExtCore.IO
open GHC.Extensions
open GHC.Domain

let potential (pizza:Pizza) minIngr maxCells =
    let pizzaSet = MutableSet.empty
    let mutable sliceLength = minIngr*2
    let mutable sliceHeight = 1
    while sliceLength>=1 do 
        for j = 1 to (Array2D.length2 pizza) do
            for i = 1 to (Array2D.length1 pizza) do    
                pizzaSet.Add ({ left = 0 ; top = 0 ; right = 0; bottom = 0; score=0}) 
        sliceLength<-sliceLength/2
        sliceHeight<-(minIngr*2)/sliceLength

let isLegalSlice (pizza:Pizza) (slice:Slice) (minIngr:int) (maxCells:int) =
   let mutable nbMushroom = 0
   let mutable nbTomato = 0
   for x=slice.left to slice.right do
      for y=slice.bottom to slice.top do
         match pizza.[x, y] with
         | M -> nbMushroom <- nbMushroom+1
         | T -> nbTomato <- nbTomato+1
   (nbMushroom >= minIngr) && (nbTomato >= minIngr) && (nbTomato+nbMushroom <= maxCells)
