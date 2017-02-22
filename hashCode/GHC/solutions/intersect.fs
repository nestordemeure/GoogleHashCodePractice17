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
   let mutable slices = allParts
