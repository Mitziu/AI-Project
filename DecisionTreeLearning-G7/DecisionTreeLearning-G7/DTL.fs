﻿open System
open MathNet.Numerics
open Microsoft.FSharp.Math

let mockAttributes = Map.empty.
                        Add("outlook",["sunny";"overcast";"rainy"]).
                        Add("temp",["hot";"mild";"cold"]).
                        Add("humidity",["high";"normal"]).
                        Add("windy",["false";"true"])

let tennisAttributes = Map.empty.
                        Add("outlook",["sunny";"overcast";"rainy"]).
                        Add("temp",["hot";"mild";"cool"]).
                        Add("humidity",["high";"normal"]).
                        Add("windy",["false";"true"])

// My test of the classification
(*let tennisExamples = System.IO.File.ReadLines(@"C:\Users\grant\Documents\Stuff\School\Spring 2018\tennis.txt") |>
                     Seq.map (fun L -> (L.Split ',') |> Array.toList) |> Seq.toList
*) 

// This is a really bad way to do it right now, but I need some map from class to its attributes,
// along a way to know which one is "positive" (or do I)? Either way, I'll treat the 0th index as the positive value.
// UPDATE: It doesn't matter
let mockClass = ["yes";"no"]

let mockExamples = [["no";"sunny";"cold";"high";"true"]; // currently guessing that we put classification at index 0 
                    ["yes";"sunny";"hot";"normal";"false"]; // this would always be consistent no matter size of data
                    ["yes";"overcast";"cold";"high";"true"];
                    ["no";"rainy";"mild";"high";"true"]]

// Do we want to add an index for the class in the mock index?
// Main issue right now is getting the class variable in -- I need to know its domain,
// but it needs to be not in (or removed) from the attributes data structure
let mockIndex = Map.empty.Add("outlook", 1).Add("temp",2).Add("humidity", 3).Add("windy", 4).Add("class",0)
let tennisIndex = Map.empty.Add("outlook", 0).Add("temp",1).Add("humidity", 2).Add("windy", 3).Add("class",4)

type DecisionTree =
    | InnerNode of attribute : string * subtrees : Map<string,DecisionTree>
    | LeafNode of classification : string

//Extracts all of the classes in an example set
//@examples: The example set you want classes from
let getClasses (examples : 'a list list) (index : Map<string,int>) =
    List.map (fun (l : 'a list) -> l.[index.["class"]]) examples

//Finds the most common class in an example set
//@examples: The example set you want classes from
let plurality (examples : 'a list list) (index : Map<string,int>) =
    let classes = getClasses examples index
    let majorityList = List.countBy (fun s -> s) classes |> List.sortBy (fun (_, y) -> -y)
    fst majorityList.Head

//Checks to see if all classes in remaining rows are the same
//@examples: The example set you want to check
let sameClassification (examples : 'a list list) (index : Map<string,int>) =
    let classes = getClasses examples index
    List.forall (fun x -> x = classes.Head) classes

//What percent of an attribute has a given value (pk + nk / p + n)
//@attribute: The attribute you are checking
//@value: The value of the attribute you are checking
//@examples: The rows in your data
let percentValue (attribute : string) (value : string) (examples : string list list) (index : Map<string,int>) =
    // First line calculates (pk + nk), second line (p + n)
    let examplesWithValue = List.filter (fun (row : string list) -> row.[index.[attribute]] = value) examples in
    (float) examplesWithValue.Length / (float) examples.Length

//Of those with an give attribute value, what percent are positive (pk / pk + nk)
//@attribute: The attribute you are checking
//@value: The value of the attribute you are checking
//@examples: The rows in your data
let percentAttributesPositive (attribute : string) (value : string) (examples : string list list) (index : Map<string,int>) = 
    // First line calculates pk, second line pk + nk
    let examplesWithValue = List.filter (fun (x : string list) -> x.[index.[attribute]] = value) examples in
    let positiveValues = List.filter (fun (x : string list) -> x.Head = mockClass.Head) examplesWithValue in
    // If statement to avoid division by 0 errors if there are no examples for a given value
    if examplesWithValue.Length = 0 then 0.0 else (float) positiveValues.Length / (float) examplesWithValue.Length

//Total percent of examples that are positive (p / p + n)
//@examples: The rows in your data
let percentPositive (examples : string list list) =
    // First line calculates p (positive values)
    let positives = List.filter (fun (x : string list) -> x.Head = mockClass.Head) examples in
        (float) positives.Length / (float) examples.Length

//Calculates the entropy of a given number
//@num: The probability of the value occuring
let entropy (num : float) =
    // If statement to avoid issues with infinity at the edge cases
    if num = 0.0 || num = 1.0 then 0.0 else - (num * Math.Log(num,2.0) + (1.0 - num) * Math.Log(1.0 - num,2.0))

//Calculates the "remainder" value from the text
//@attribute: The attribute to calculate the remainder for
//@attributes: List of attributes
//@examples: Rows in the data
let remainder (attribute : string) (attributes : Map<string,string list>) (examples : string list list) (index : Map<string,int>) =
    let values = attributes.[attribute] in
    // This whole list shenanigans does the sum of (pk + nk / p + n) * entropy(pk / pk + nk)
    List.map (fun (value : string) ->
        (percentValue attribute value examples index) * (entropy (percentAttributesPositive attribute value examples index))) values |>
        List.sum

//Calculates the information gain of choosing a particular attribute
//@attribute: The attribute to calculate information gain for
//@attributes: List of attributes
//@examples: Rows in the data
let infoGain (attribute : string) (attributes : Map<string,string list>) (examples : string list list) (index : Map<string,int>) =
    (entropy (percentPositive examples)) - (remainder attribute attributes examples index)

//Selects the attribute with the highest information gain
//@attributes: List of attributes
//@examples: Rows in the data
let mostImportant (attributes : Map<string,string list>) (examples : string list list) (index : Map<string,int>): string =
   // Get attributes in list form, calculate their gains, zip them together, sort, and pick the best one
   let attributeList = Map.toList attributes |> List.map (fun x -> fst x) in
   let gains = List.map (fun (attribute : string) -> infoGain attribute attributes examples index) attributeList in
   let zipped = List.zip attributeList gains |> List.sortBy (fun (_, y) -> -y) in
   fst zipped.Head
 
//Pure black wizard voodoo magic. A mutually recursive function that uses functions above to construct the tree itself
//@examples: Rows of data
//@attributes: List of attributes (backwards from rest of functions but I'm scared to change until committed)
//@parentexamples: The examples from the parent tree
let rec createSubtree (examples : string list list) (attributes : Map<string, string list>) (index : Map<string,int>) : DecisionTree = 
    let attribute = mostImportant attributes examples index in // The attribute we're selecting to split on
    let values = attributes.[attribute] in // List of values for the attribute
    // Creates a list, that has one list for each value, which is a list of rows (themselves lists) that contain the given value
    let exs = List.map (fun value -> List.filter (fun (row : string list) ->
        row.[index.[attribute]] = value) examples) values in
    // A new attribute map, without the attribute we're splitting on
    let fewerAttributes = Map.remove attribute attributes in
    // Create a list of subtrees (by calling dtl) with the values created above
    let subtrees = List.map (fun newexs -> dtl newexs fewerAttributes examples index) exs in
    // Match the values with their subtree in a list of tuples
    let zipped = List.zip values subtrees in
    // Convert the list of tuples into a map, and make create the InnerNode for the split
    InnerNode(attribute,Map.ofList(zipped))

// HOW TO CALL : dtl examples attributes [] indexMap
and dtl (examples : string list list) (attributes : Map<string,string list>) (parentExamples : string list list) (index : Map<string,int>) =
    // This is boring, but handles the end cases where we run out of examples, attributes, or have a unanimous node.
    if examples.IsEmpty then LeafNode (plurality parentExamples index)
    elif sameClassification examples index then LeafNode (examples.Head.[index.["class"]])
    elif Map.isEmpty attributes then LeafNode (plurality parentExamples index)
    else createSubtree examples attributes index

// Stolen from Mitziu's code because I've been going at this for hours and don't want to deal with imports and git right now

 //Classify Individual Row
//@node: Current Node in the decision Tree
//@row: Current row being classified
//@indexMap: Map that maps attribute names to Index
let rec classify (node:DecisionTree) (row:string list) (indexMap: Map<string, int>): string=
    match node with
    | LeafNode decision -> decision
    | InnerNode (attribute, attributeMap) -> 
        //Obtains the correct Index
        let index = (Map.find attribute indexMap)
        //Extracts the attribute value
        let attributeValue = row.[index]
        //Loads the correct child node
        let newNode = (Map.find attributeValue attributeMap)
        //Recursively traverses to the next correct child node
        classify newNode row indexMap
   
//Classify All of the data
//@data: Entire data set to be classified
//@root: Root of Decision Tree
//@indexMap: Map that maps attribute names to Index
let classifyAllRows (data:string list list) (root:DecisionTree) (indexMap: Map<string, int>): string list =
    data |> List.map (fun row -> classify root row indexMap)

//Will return the accruacy for the testing data set
//@testData: Entire data set to be classified
//@root: Root of Decision Tree
//@indexMap: Map that maps attribute names to Index
//@groundTruth: List of strings that will be used to check accuracy
let testAccuracy (testData:string list list) (root:DecisionTree) (indexMap: Map<string, int>) = 
    let testDataResults = classifyAllRows testData root indexMap
    let groundTruth = testData |> List.map (fun row -> row.[indexMap.["class"]])
    printfn "%A" testDataResults
    printfn "%A" groundTruth
    let result = List.map2 (fun a b -> a = b) testDataResults groundTruth
    let accuracy = float (result |> List.sumBy (fun x -> if x = true then 1 else 0)) / float (List.length result)
    accuracy


type Option<'a> = 
    | Some of 'a
    | None

let rec toMap (node:DecisionTree): (Map<string,Option<'_a>>)=
    match node with
    //| LeafNode decision -> [("a", LeafNode decision)]
    //| LeafNode decision -> Map.empty.Add("Yes", null);
    | LeafNode s-> Map.empty.Add(s, None)
    | InnerNode (attribute, attributeMap) -> 
        let children = attributeMap |> Map.toList
        let children = children |> List.map (fun (k,v) -> (k, (toMap (v)))) |> Map.ofList
        Map.empty.Add(attribute, Some children)


let Yes = LeafNode("yes")
let No = LeafNode("no")

let Humidity = InnerNode("humidity", Map.empty.Add("high", No).Add("normal", Yes))

let Wind = InnerNode("wind", Map.empty.Add("true", No).Add("false", Yes))

let Outlook = InnerNode("outlook", Map.empty.Add("sunny", Humidity).Add("overcast", Yes).Add("rainy", Wind))
let root = Outlook;