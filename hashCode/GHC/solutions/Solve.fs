module GHC.Solve

open ExtCore.Collections

open GHC.Extensions
open GHC.Extensions.Common
open GHC.Domain

open GHC.Potential
open GHC.Intersect

//-------------------------------------------------------------------------------------------------



//-------------------------------------------------------------------------------------------------
// SOLUTION

/// solution
let solution (pizza:Pizza) minIngr maxCells = 
   let allParts = potential pizza minIngr maxCells
   intersect allParts
