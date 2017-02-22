module GHC.Intersect

open ExtCore.Collections
open ExtCore.IO
open GHC.Extensions
open GHC.Domain
open System.Collections.Generic

//-------------------------------------------------------------------------------------------------
// SOLUTION1
let legal (slice1:Slice) (slice2:Slice) = 
   slice1.bottom > slice2.top
   || slice1.top < slice2.bottom
   || slice1.left > slice2.right
   || slice1.right < slice2.left

let inline conflict (slice1:Slice) (slice2:Slice) = not (legal slice1 slice2)

//-------------------------------------------------------------------------------------------------

let intersect (allParts:MutableSet<Slice>) =
   /// list of all slices from the biggest to the smallest
   let slices = 
      allParts 
      |> List.ofSeq
      |> List.sortByDescending (fun slice -> slice.score)
      //|> List.sortBy (fun slice -> slice.score)
   /// get the biggest, eliminate conflicts, etc...
   let rec greed slices solution =
      match slices with 
      | [] -> solution
      | bestSlice :: q -> 
         printf "."
         let newSolution = bestSlice :: solution 
         let newSlices = List.filter (conflict bestSlice >> not) slices
         greed newSlices newSolution
   greed slices []

//-------------------------------------------------------------------------------------------------
// SOLUTION2

type Case = { bottomRight : Slice list ; topRight : Slice list ; bottomLeft : Slice list ; topLeft : Slice list}

let emptyCase = {bottomRight=[];topRight=[];bottomLeft=[];topLeft=[]}

let fillCases rowNumber colNumber (allParts:MutableSet<Slice>) =
   let cases = Array2D.create rowNumber colNumber emptyCase
   for slice in allParts do 
      let caseBR = cases.[slice.bottom, slice.right]
      cases.[slice.bottom, slice.right] <- {caseBR with bottomRight= slice :: caseBR.bottomRight}
      //cases.[slice.bottom, slice.left].bottomLeft <- slice
      //cases.[slice.top, slice.left].topLeft <- slice
      //cases.[slice.top, slice.right].topRight <- slice
   cases

//-------------------------------------------------------------------------------------------------

let purgeSlice slice (cases:Case[,]) =
   for r = slice.left to slice.right do 
      for c = slice.top to slice.bottom do
         cases.[r,c] <- emptyCase

let intersect2 (pizza:Pizza) (allParts:MutableSet<Slice>) =
   let rowNumber = Array2D.length1 pizza
   let colNumber = Array2D.length2 pizza
   printfn "filling the cases..."
   let cases = fillCases rowNumber colNumber allParts
   printfn "going around in circles..."
   let mutable result = []
   let pathBL =
      [ 
         for r = 0 to rowNumber-1 do
            for c = 0 to colNumber-1 do 
               yield (r,c)
      ]
   let pathTL =
      [ 
         for c = 0 to colNumber-1 do
            for r = rowNumber-1 to 0 do
               yield (r,c)
      ]
   let pathTR =
      [ 
         for r = rowNumber-1 to 0 do
            for c = colNumber-1 to 0 do 
               yield (r,c)
      ]
   let pathBR =
      [ 
         for c = colNumber-1 to 0 do
            for r = 0 to rowNumber-1 do
               yield (r,c)
      ]
   let rec readBL pathBL pathTL pathTR pathBR =
      match pathBL with 
      | [] -> readTL pathBL pathTL pathTR pathBR
      | (r,c)::q ->
         match cases.[r,c].bottomLeft with 
         | [] -> readBL q pathTL pathTR pathBR
         | slices -> 
            let bestSlice = List.maxBy (fun slice -> slice.score) slices
            result <- bestSlice :: result
            purgeSlice bestSlice cases
            readTL q pathTL pathTR pathBR
   and readTL pathBL pathTL pathTR pathBR =
      match pathTL with 
      | [] -> readTR pathBL pathTL pathTR pathBR
      | (r,c)::q ->
         match cases.[r,c].topLeft with 
         | [] -> readTL pathBL q pathTR pathBR
         | slices -> 
            let bestSlice = List.maxBy (fun slice -> slice.score) slices
            result <- bestSlice :: result
            purgeSlice bestSlice cases
            readTR pathBL q pathTR pathBR
   and readTR pathBL pathTL pathTR pathBR =
      match pathTR with 
      | [] -> readBR pathBL pathTL pathTR pathBR
      | (r,c)::q ->
         match cases.[r,c].topRight with 
         | [] -> readTR pathBL pathTL q pathBR
         | slices -> 
            let bestSlice = List.maxBy (fun slice -> slice.score) slices
            result <- bestSlice :: result
            purgeSlice bestSlice cases
            readBR pathBL pathTL q pathBR
   and readBR pathBL pathTL pathTR pathBR =
      match pathBR with 
      | [] -> ()
      | (r,c)::q ->
         match cases.[r,c].bottomRight with 
         | [] -> readBR pathBL pathTL pathTR q
         | slices -> 
            let bestSlice = List.maxBy (fun slice -> slice.score) slices
            result <- bestSlice :: result
            purgeSlice bestSlice cases
            readBL pathBL pathTL pathTR q
   readBL pathBL pathTL pathTR pathBR
   result