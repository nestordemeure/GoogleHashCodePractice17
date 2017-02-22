module GHC.Main

open ExtCore.Collections

open GHC.Extensions
open GHC.Extensions.Common
open GHC.Domain
open GHC.Import
open GHC.Solve
open GHC.Export
open System.Collections.Generic

//-------------------------------------------------------------------------------------------------
// EVALUATION

//let mutable score = 0

//let evaluation solution = ()

//-------------------------------------------------------------------------------------------------
// MAIN

[<EntryPoint>]
let main argv =
    // import
    let inPath = "../inputs/medium.in"
    let pizza, minIngr, maxCells = import inPath
    // solution

    // evaluation
    //evaluation r
    //printfn "score : %d" score
    //export 
    export "../output.txt" [||]
    0 // return an integer exit code
