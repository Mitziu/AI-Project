﻿// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System.IO
open FSharp.Data
open FSharp.Data.JsonExtensions
open System.Runtime.Serialization.Formatters
open System.Runtime.Serialization.Formatters
open System.Security.Cryptography
open System.ComponentModel
open FSharp.Json
open System.Diagnostics
open System.Collections.Generic

[<Literal>]
let baseDirectory = __SOURCE_DIRECTORY__
[<Literal>]
let jsonFile1 = "\\Data\\tic-tac-toe.json"
[<Literal>]
let jsonFile2 = "\\Data\\house-votes.json"
[<Literal>]
let jsonFile3 = "\\Data\\breast-cancer.json"
[<Literal>]
let jsonFile4 = "\\Data\\monk.json"

[<Literal>]
let jsPath1 = baseDirectory + jsonFile1
[<Literal>]
let jsPath2 = baseDirectory + jsonFile2 
[<Literal>]
let jsPath3 = baseDirectory + jsonFile3 
[<Literal>]
let jsPath4 = baseDirectory + jsonFile4


type JsonData = JsonProvider<jsPath4>
let doc = JsonData.GetSample()

// for splitting attributes
let splitAttr (text: string) = 
    let mutable cleanText = text
    cleanText <- cleanText.Replace("(", "")
    cleanText <- cleanText.Replace(")", "")
    cleanText <- cleanText.Replace("\"", "")
    let word = cleanText.Split ','
    word

// for splitting domains
let splitDomain (text: string) = 
    let mutable cleanText = text
    cleanText <- cleanText.Replace("(", "")
    cleanText <- cleanText.Replace(")", "")
    cleanText <- cleanText.Replace("\"", "")
    cleanText <- cleanText.Replace("\"", "")
    let word = cleanText.Split '['
    word

// FOR ATTRIBUTES PROCESSING
let attrs = doc.Attributes.JsonValue.Properties

let attrList = new List<string>()
for attr in attrs do 
   attrList.Add(attr.ToString())


let mutable attrMap = Map.empty
let mutable tempMap = Map.empty
for item in attrList do 
    //printfn "testing"
    let result = splitAttr item //splitLine item
    attrMap <- attrMap.Add((result.[1]).Trim(), (result.[0]).Trim())
    tempMap <- tempMap.Add((result.[0]).Trim(), (result.[1]).Trim())


// FOR DOMAIN PROCESSING
let domains = doc.Domain.JsonValue.Properties

let domainList = new List<string>()
for domain in domains do 
    domainList.Add(domain.ToString())

let mutable domMap = Map.empty
printfn "testing"

for item in domainList do 
    let result = splitDomain item
    // printfn "%A" result
    //printfn "%s" ((result.[0].Replace(",", "").Trim()))
    let key = result.[0].Replace(",", "").Trim()
    //printfn "%s" (result.[2])
    //printfn "%s" (tempMap.Item(key))
    let value = result.[1].Replace("]", "").Trim().Split ',' |> List.ofArray
   //printfn "%A" value

    domMap <- domMap.Add(tempMap.Item(key), value)

// method for converting sequece to list
let readLines filePath = Seq.toList (System.IO.File.ReadLines(filePath))

(*

let shift (array: 'a[]) = 
    let cat = array.[0]
    for i in 1..array.Length-1 do 
        array.[i-1] <- array.[i]
    array.[array.Length-1] <- cat
    array


//1:  tic-tac-toe
let filePath1 = "Data\\tic-tac-toe.txt"
let fullPath1 = Path.Combine(baseDirectory, filePath1)


let result1' = readLines fullPath1 
let result1 = result1' |> List.map (fun l -> l.Split ',' |> Array.toList) 


// 2: house-votes
let filePath2 = "Data\\house-votes.txt"
let fullPath2 = Path.Combine(baseDirectory, filePath2)
let result2' = readLines fullPath2 
let result2 = result2' |> List.map (fun a -> a.Split ','|> fun arr -> shift arr |> Array.toList)


// 3. breast-cancer 

let filePath3 = "Data\\breast-cancer.txt" 
let fullPath3 = Path.Combine(baseDirectory, filePath3)
let result3' = readLines fullPath3
let result3 = result3' |> List.map (fun a -> a.Split ',' |> fun arr -> shift arr |> Array.toList)


// 4. monk 

let filePath4 = "Data\\monk.txt"
let fullPath4 = Path.Combine(baseDirectory, filePath4)
let result4' = readLines fullPath4
let result4 = result4' |> List.map (fun a -> a.Split ' ' |> Array.filter ((<>) "")|> fun arr -> shift arr |> Array.toList)
*)



[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
