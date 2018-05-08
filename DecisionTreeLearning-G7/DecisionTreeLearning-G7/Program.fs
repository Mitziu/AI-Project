// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System.IO
open FSharp.Data
open FSharp.Data.JsonExtensions

[<Literal>]
let baseDirectory = __SOURCE_DIRECTORY__
[<Literal>]
let jsonFile1 = "\\Data\\tic-tac-toe.json"
[<Literal>]
let jsonFile2 = "\\Data\\balance.json"
[<Literal>]
let jsonFile3 = "\\Data\\breast-cancer.json"
[<Literal>]
let jsPath1 = baseDirectory + jsonFile1
[<Literal>]
let jsPath2 = baseDirectory + jsonFile2 
[<Literal>]
let jsPath3 = baseDirectory + jsonFile3 



type JsonData = JsonProvider<jsPath3>
let doc = JsonData.GetSample()
printfn "Testing"
printfn "%s" doc.Attributes.``0`` // attribute name by value
let list = doc.Domain.``0``
printfn "%A" list // attribute index 0's domain


let readLines filePath = Seq.toList (System.IO.File.ReadLines(filePath))

(*
//1:  tic-tac-toe
let filePath1 = "Data\\tic-tac-toe.txt"
let fullPath1 = Path.Combine(baseDirectory, filePath1)

// method for converting sequece to list
let result1' = readLines fullPath1 
let result1 = result1' |> List.map (fun l -> l.Split ',' |> Array.toList) 
*)
(*
let swap (array: 'a[]) (first: int) (second: int): 'a[] = 
    let originalFirst = array.[first]
    array.[first] <- array.[second];
    array.[second] <- originalFirst;
    array
*)
// 2. balance 

let shift (array: 'a[]) = 
    let cat = array.[0]
    for i in 1..array.Length-1 do 
        array.[i-1] <- array.[i]
    array.[array.Length-1] <- cat
    array
(*
let filePath2 = "Data\\balance.txt"
let fullPath2 = Path.Combine(baseDirectory, filePath2)
let result2' = readLines fullPath2 
let result2 = result2' |> List.map (fun a -> a.Split ','|> fun arr -> shift arr |> Array.toList)
*)

// 3. breast-cancer 
(*
let filePath3 = "Data\\breast-cancer.txt" 
let fullPath3 = Path.Combine(baseDirectory, filePath3)
let result3' = readLines fullPath3
let result3 = result3' |> List.map (fun a -> a.Split ',' |> fun arr -> shift arr |> Array.toList)
*)




[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
