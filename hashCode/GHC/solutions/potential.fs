
module GHC.Potential

open ExtCore.Collections
open ExtCore.IO
open GHC.Extensions
open GHC.Domain

let checkSliceInside (slice:Slice) (pizza:Pizza) =
    (   slice.left>0 &&
        slice.bottom>0 &&
        slice.right<Array2D.length2 pizza &&
        slice.top<Array2D.length1 pizza)


let potential (pizza:Pizza) minIngr maxCells =
    let sliceSet = MutableSet.empty
    let mutable sliceLength = minIngr*2
    let mutable sliceHeight = 1
    while sliceLength>=1 do
        for j = 0 to ((Array2D.length1 pizza)-1) do
            for i = 0 to ((Array2D.length2 pizza)-1) do
                let sliceA = { left = i ; top = j-sliceHeight ; right = i+sliceLength; bottom = j; score=minIngr*2}
                let sliceB = { left = i ; top = j ; right = i+sliceLength; bottom = j+sliceHeight; score=minIngr*2}
                let sliceC = { left = i-sliceLength ; top = j ; right = i; bottom = i+sliceLength; score=minIngr*2}
                let sliceD = { left = i-sliceLength ; top = j-sliceHeight ; right = i; bottom = j; score=minIngr*2}
                if (checkSliceInside sliceA pizza) then MutableSet.add sliceA sliceSet
                if (checkSliceInside sliceB pizza) then MutableSet.add sliceB sliceSet
                if (checkSliceInside sliceC pizza) then MutableSet.add sliceC sliceSet
                if (checkSliceInside sliceD pizza) then MutableSet.add sliceD sliceSet
        sliceLength<-sliceLength/2
        sliceHeight<-(minIngr*2)/sliceLength
    sliceSet

let isLegalSlice (pizza:Pizza) (slice:Slice) (minIngr:int) (maxCells:int) =
   let mutable nbMushroom = 0
   let mutable nbTomato = 0
   for x=slice.left to slice.right do
       for y=slice.bottom to slice.top do
           match pizza.[x, y] with
           | M -> nbMushroom <- nbMushroom+1
           | T -> nbTomato <- nbTomato+1
   (nbMushroom >= minIngr) && (nbTomato >= minIngr) && (nbTomato+nbMushroom <= maxCells)
