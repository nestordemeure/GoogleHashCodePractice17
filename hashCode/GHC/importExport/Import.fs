module GHC.Import

open System.IO
open ExtCore.Collections
open ExtCore.IO

open GHC.Extensions.Common
open GHC.Extensions.Scanf
open GHC.Extensions
open GHC.Domain

//-------------------------------------------------------------------------------------------------



//-------------------------------------------------------------------------------------------------
// IMPORT

let import path =
   // File.ReadLines(path)
   let text = File.ReadAllLines(path)
   let rowNumber, colNumber, minIngr, maxCells = sscanf "%d %d %d %d" text.[0]
   let pizza = Array2D.create rowNumber colNumber M
   for i = 1 to 1 + rowNumber - 1 do 
      let index = i - 1
      for c = 0 to colNumber-1 do 
         let ingr = if text.[i].[c] = 'T' then T else M
         pizza.[index,c] <- ingr
   pizza, minIngr, maxCells