module GHC.Export

open ExtCore.Collections
open System.IO

open GHC.Extensions
open GHC.Domain

//-------------------------------------------------------------------------------------------------



//-------------------------------------------------------------------------------------------------
// EXPORTATION

let export path (slices:Slice list) =
   let sliceNumber = List.length slices |> string
   let lines =
      slices
      |> List.map (fun slice -> sprintf "%d %d %d %d" slice.top slice.bottom slice.right slice.left)
   File.WriteAllLines(path, sliceNumber :: lines)
