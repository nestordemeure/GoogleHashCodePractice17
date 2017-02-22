module GHC.Domain

open ExtCore.Collections
open System.Collections.Generic

open GHC.Extensions
open GHC.Extensions.Common

//-------------------------------------------------------------------------------------------------

//type graph = Dictionary<'key,'Node>
type Ingr = M | T

type Pizza = Ingr [,]

/// legal if au moins minIngr  de chaque ingr et maxCells en tout de pizza
type Slice = { left : int ; top : int ; right : int ; bottom : int ; score : int}


type PotentialsSlices = MutableSet<Slice>

//-------------------------------------------------------------------------------------------------
