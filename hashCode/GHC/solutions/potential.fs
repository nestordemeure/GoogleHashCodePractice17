
module GHC.Potential

open ExtCore.Collections
open ExtCore.IO
open GHC.Extensions
open GHC.Domain

let checkPizzaInside (slice:Slice) (pizza:Pizza) =
    (   slice.left>0 &&
        slice.bottom>0 &&
        slice.right<Array2D.length2 pizza &&
        slice.top<Array2D.length1 pizza)


let potential (pizza:Pizza) minIngr maxCells =
    let pizzaSet = MutableSet.empty
    let mutable sliceLength = minIngr*2
    let mutable sliceHeight = 1
    while sliceLength>=1 do
        for j = 0 to (Array2D.length2 pizza) do
            for i = 0 to (Array2D.length1 pizza) do
                let slice = { left = i ; top = sliceHeight ; right = sliceLength; bottom = j; score=minIngr*2}

                let slice = { left = i ; top = j ; right = sliceLength; bottom = sliceHeight; score=minIngr*2}

                let slice = { left = sliceLength ; top = j ; right = i; bottom = sliceLength; score=minIngr*2}

                let slice = { left = sliceLength ; top = sliceHeight ; right = i; bottom = j; score=minIngr*2}


                pizzaSet.Add ()
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
