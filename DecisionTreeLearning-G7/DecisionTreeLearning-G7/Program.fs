// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System.IO
open FSharp.Data
open FSharp.Data.JsonExtensions
open System.Collections.Generic
open System
// gets base path to be used fo relative path 
[<Literal>]
let baseDirectory = __SOURCE_DIRECTORY__
[<Literal>]

// 5 json files path 
let jsonFile1 = "\\Data\\tic-tac-toe.json"
[<Literal>]
let jsonFile2 = "\\Data\\house-votes.json"
[<Literal>]
let jsonFile3 = "\\Data\\breast-cancer.json"
[<Literal>]
let jsonFile4 = "\\Data\\monk.json"
[<Literal>]
let jsonFile5 = "\\Data\\spect.json"

// path for the json files
[<Literal>]
let jsPath1 = baseDirectory + jsonFile1
[<Literal>]
let jsPath2 = baseDirectory + jsonFile2 
[<Literal>]
let jsPath3 = baseDirectory + jsonFile3 
[<Literal>]
let jsPath4 = baseDirectory + jsonFile4
[<Literal>]
let jsPath5 = baseDirectory + jsonFile5

// for splitting attributes
let splitAttr (text: string) = 
    let mutable cleanText = text
    cleanText <- cleanText.Replace("(", "").Replace(")", "").Replace("\"", "")
    let word = cleanText.Split ','
    word

// for splitting domains
let splitDomain (text: string) = 
    let mutable cleanText = text
    cleanText <- cleanText.Replace("(", "").Replace(")", "").Replace("\"", "").Replace("]", "").Replace("\r\n", "")
    let word = cleanText.Split '['
    //printfn "%A" word
    word

// for mapping domains and attributes
// @jsonFilePath: path of the json file
let domainMapping jsonFilePath = 
    //type JsonData = JsonProvider<jsonFilePath>
    //printfn "testing"
    let data = File.ReadAllText(jsonFilePath)
    let doc = JsonValue.Parse(data)
    //printfn "testing"
    //printfn "%s" (doc.ToString())
 
    let attributes = doc?attributes
    let attrs = attributes.Properties

    let attrList = new List<string>()
    for attr in attrs do 
        attrList.Add(attr.ToString())
    let mutable indexMap = Map.empty
    let mutable tempMap = Map.empty

    for item in attrList do 
        //printfn "testing"
        let result = splitAttr item //splitLine item
        indexMap <- indexMap.Add((result.[1]).Trim(), (result.[0]).Trim() |> int)
        tempMap <- tempMap.Add((result.[0]).Trim() |> int, (result.[1]).Trim())
    //printfn "printing indexMap"
    //printfn "%A" indexMap
    
    // FOR DOMAIN PROCESSING
    let dom = doc?domain
    let domains = dom.Properties
    let domainList = new List<string>()
    for domain in domains do 
       domainList.Add(domain.ToString())
    let mutable domMap = Map.empty
    //printfn "testing"
    for item in domainList do 
        //printfn "%s" item
        let result = splitDomain item
        //printfn "%s" result.[0]
        let key = result.[0].Replace(",", "").Trim() |> int
        //printfn "%s" (tempMap.Item(key))
        //printfn "%s" (result.[1])
        let value = result.[1].Split ',' |> Array.toList |> List.map (fun x -> x.Trim())
        //printfn "%A" value
        domMap <- domMap.Add(tempMap.Item(key), value)
    (domMap, indexMap)
   

// method for converting sequece to list
let readLines filePath = Seq.toList (System.IO.File.ReadLines(filePath))
let shift (array: 'a[]) = 
    let cat = array.[0]
    for i in 1..array.Length-1 do 
        array.[i-1] <- array.[i]
    array.[array.Length-1] <- cat
    array

// get dataset for tic-tac-toe
// @datasetFileName: name of a dataset
let getTTTData datasetFileName = 
    //1:  tic-tac-toe
    let filePath1 = "Data\\" + datasetFileName
    let fullPath1 = Path.Combine(baseDirectory, filePath1)
    let result1' = readLines fullPath1 
    let result1 = result1' |> List.map (fun l -> l.Split ',' |> Array.toList) 
    result1

// get dataset for monk
// @datasetFileName: name of a dataset
let getMonkData datasetFileName = 
    // 4. monk 
    let filePath4 = "Data\\" + datasetFileName
    let fullPath4 = Path.Combine(baseDirectory, filePath4)
    let result4' = readLines fullPath4
    let result4 = result4' |> List.map (fun a -> a.Split ' ' |> Array.filter ((<>) "")|> fun arr -> shift arr |> Array.toList)
    result4

// get dataset for spect, house-votes & breast-cancer 
// @datasetFileName: name of a dataset
let getSHVBCData datasetFileName = 
    // 2: house-votes
    let filePath2 = "Data\\" + datasetFileName
    let fullPath2 = Path.Combine(baseDirectory, filePath2)
    let result2' = readLines fullPath2 
    let result2 = result2' |> List.map (fun a -> a.Split ','|> fun arr -> shift arr |> Array.toList)
    result2
// matches dataset file name w/ it's appropriate data parser
// @datasetFileName: name of a dataset
let getSanitizedData datasetFileName = 
    let mutable dataset = []
    if (datasetFileName = "tic-tac-toe.txt") then  dataset <- (getTTTData datasetFileName)
    else if (datasetFileName = "monk.txt") then dataset <- (getMonkData datasetFileName)
    else if (datasetFileName = "house-votes.txt" || datasetFileName = "breast-cancer.txt" || datasetFileName = "spect.txt") then dataset <- (getSHVBCData datasetFileName)
    else
        printfn "No dataset found with the provided filename." 
    dataset

// maps json file name w/ its appropriate file path and calls method for getting map of domain and attributes
// @jsonFileName: name of a json file
let getMaps jsonFileName = 
    let mutable path = ""
    if (jsonFileName = "tic-tac-toe.json") then path <- jsPath1
    else if (jsonFileName = "monk.json") then path <- jsPath4
    else if (jsonFileName = "house-votes.json") then path <- jsPath2
    else if (jsonFileName = "breast-cancer.json") then path <- jsPath3
    else if (jsonFileName = "spect.json") then path <- jsPath5
    else
        printfn "No json file found with the provided filename." 
 
    let attDomTup = domainMapping path
    attDomTup

[<EntryPoint>]
let main argv = 
    
    let jsonFile = argv.[0].Trim()
    let dataFile = argv.[1].Trim()
    let dataset = getSanitizedData dataFile 
    let domMap, indexMap = getMaps jsonFile
    let attrMap = Map.remove "class" domMap 

    //printfn "%A" attrMap
    //printfn "%A" indexMap
    //printfn "%A" dataset
    //printfn "%s" jsonFile
    //printfn "%s" dataFile
    
    let dtlTest = DTL.dtl dataset attrMap [] indexMap -1 false 
    //let bestModel = DTL.find_best_model dataset 4 attrMap indexMap
    //let accuracy = DTL.k_fold_validation dataset 4 attrMap indexMap false -1
    printfn "%O" dtlTest
    //printfn "%f %f" (fst accuracy) (snd accuracy)
    //printfn "%A" (domMap.Item("a1"))
    //printfn "%A" argv
    0 // return an integer exit code