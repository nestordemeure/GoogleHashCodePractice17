
module GHC.Potential

open ExtCore.Collections
open ExtCore.IO
open GHC.Extensions
open GHC.Domain

let checkSliceInside (slice:Slice) (pizza:Pizza) =
    (   slice.left>0 && slice.right < Array2D.length2 pizza &&
        slice.top>0 && slice.bottom < Array2D.length1 pizza &&
        slice.right>0 && slice.right<Array2D.length2 pizza &&
        slice.bottom>0 && slice.bottom<Array2D.length1 pizza &&
        slice.bottom>slice.top &&
        slice.right>slice.left
    )

let isLegalSlice (slice:Slice) (pizza:Pizza) (minIngr:int) (maxCells:int) =
   let mutable nbMushroom = 0
   let mutable nbTomato = 0
   for x=slice.left to slice.right do
       for y=slice.bottom to slice.top do
           match pizza.[x, y] with
           | M -> nbMushroom <- nbMushroom+1
           | T -> nbTomato <- nbTomato+1
   (checkSliceInside slice pizza) &&
   (nbMushroom >= minIngr) && (nbTomato >= minIngr) && (nbTomato+nbMushroom <= maxCells)


let potential (pizza:Pizza) minIngr maxCells =
    let sliceSet = MutableSet.empty
    let mutable sliceLength = minIngr*2
    let mutable sliceHeight = 1
    while sliceLength>=1 do
        //printfn "%d" sliceLength
        //System.Console.ReadLine() |> ignore
        for j = 0 to ((Array2D.length1 pizza)-1) do
            for i = 0 to ((Array2D.length2 pizza)-1) do
                let sliceA = { left = i ; top = j-sliceHeight ; right = i+sliceLength; bottom = j; score=minIngr*2}
                let sliceB = { left = i-sliceHeight ; top = j-sliceLength ; right = i; bottom = j; score=minIngr*2}
                let sliceC = { left = i-sliceLength ; top = j ; right = i; bottom = i+sliceHeight; score=minIngr*2}
                let sliceD = { left = i ; top = j ; right = i+sliceHeight; bottom = j+sliceLength; score=minIngr*2}
                if (isLegalSlice sliceA pizza minIngr maxCells) then MutableSet.add sliceA sliceSet
                if (isLegalSlice sliceB pizza minIngr maxCells) then MutableSet.add sliceB sliceSet
                if (isLegalSlice sliceC pizza minIngr maxCells) then MutableSet.add sliceC sliceSet
                if (isLegalSlice sliceD pizza minIngr maxCells) then MutableSet.add sliceD sliceSet
        sliceLength<-sliceLength/2
        if(sliceLength<>0) then sliceHeight<-(minIngr*2)/sliceLength
    sliceSet
