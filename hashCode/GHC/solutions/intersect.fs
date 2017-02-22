module GHC.Intersect

open ExtCore.Collections
open ExtCore.IO
open GHC.Extensions
open GHC.Domain
open System.Collections.Generic

//-------------------------------------------------------------------------------------------------

let legal (slice1:Slice) (slice2:Slice) = 
   slice1.bottom > slice2.top
   || slice1.top < slice2.bottom
   || slice1.left > slice2.right
   || slice1.right < slice2.left

let inline conflict (slice1:Slice) (slice2:Slice) = not (legal slice1 slice2)

//-------------------------------------------------------------------------------------------------

let intersect2 (allParts:MutableSet<Slice>) =
   /// list of all slices from the biggest to the smallest
   let slices = allParts |> Set.ofSeq
   let illegalSlices =
      slices
   /// get the biggest, eliminate conflicts, etc...
   let rec greed slices solution =
      match slices with 
      | [] -> solution
      | bestSlice :: q -> 
         let newSolution = bestSlice :: solution 
         let newSlices = List.filter (conflict bestSlice >> not) slices
         greed newSlices newSolution
   greed slices []

//-------------------------------------------------------------------------------------------------

let intersect (allParts:MutableSet<Slice>) =
   /// list of all slices from the biggest to the smallest
   let slices = 
      allParts 
      |> List.ofSeq
      |> List.sortByDescending (fun slice -> slice.score)
   /// get the biggest, eliminate conflicts, etc...
   let rec greed slices solution =
      match slices with 
      | [] -> solution
      | bestSlice :: q -> 
         let newSolution = bestSlice :: solution 
         let newSlices = List.filter (conflict bestSlice >> not) slices
         greed newSlices newSolution
   greed slices []
