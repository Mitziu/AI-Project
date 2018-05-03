﻿// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open FSharp.Data
open FSharp.Data.CsvExtensions
(*
type DataModel = JsonProvider<"C:\\Users\\dichha\\Documents\\Graduate\\Spring2018\\AI\\final-project\\AI-Project\\DecisionTreeLearning-G7\\DecisionTreeLearning-G7\\data\\tic-tac-toe.json">
let doc = DataModel.GetSample()
printfn "Testing"
printfn "%s" (doc.Attributes.``0``)
*)
let data = CsvFile.Load("C:\\Users\\dichha\\Documents\\Graduate\\Spring2018\\AI\\final-project\\AI-Project\\DecisionTreeLearning-G7\\DecisionTreeLearning-G7\\data\\tic-tac-toe.csv");


for row in data.Rows do 
   printfn "(%s)" (row.GetColumn "0")


[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
