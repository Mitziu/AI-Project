// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System.IO
open FSharp.Data



let jsonFile = "Data\\tic-tac-toe.json"
type JsonData = JsonProvider<Sample=const(__SOURCE_DIRECTORY__ + "\\Data\\tic-tac-toe.json")>
let doc = JsonData.GetSample()
printfn "%s" doc.Attributes.``0`` // attribute name by value
let list = doc.Domain.``0``
printfn "%A" list // attribute index 0's domain



let baseDirectory = __SOURCE_DIRECTORY__
let filePath = "Data\\tic-tac-toe.txt"
let fullPath = Path.Combine(baseDirectory, filePath)

// method for converting sequece to list

let readLines filePath = Seq.toList (System.IO.File.ReadLines(filePath))
let result' = readLines fullPath 
let result = result' |> List.map (fun l -> l.Split ',' |> Array.toList) 


[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
