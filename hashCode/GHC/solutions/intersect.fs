module GHC.Intersect

open ExtCore.Collections
open ExtCore.IO
open GHC.Extensions
open GHC.Domain

//-------------------------------------------------------------------------------------------------

let conflict slice1 slice2 = 
   true

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
