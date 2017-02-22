module GHC.Solve

open ExtCore.Collections

open GHC.Extensions
open GHC.Extensions.Common
open GHC.Domain

//-------------------------------------------------------------------------------------------------



//-------------------------------------------------------------------------------------------------
// SOLUTION

/// solution
let solution (pizza:Pizza) minIngr maxCells = 
   let allParts = potential pizza minIngr maxCells
   intersect allParts
