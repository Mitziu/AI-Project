#if INTERACTIVE
#else
module DTL
#endif
open System 
open System.IO
open FSharp.Json
open MathNet.Numerics
open MathNet.Numerics.Distributions
open MathNet.Numerics.Random
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
let tennisExamples = System.IO.File.ReadLines(@"C:\Users\Mitziu\source\repos\AI-Project2\DecisionTreeLearning-G7\DecisionTreeLearning-G7\data\tennis.txt") |>
                     Seq.map (fun L -> (L.Split ',') |> Array.toList) |> Seq.toList


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
    | InnerNode of attribute : string * subtrees : Map<string,DecisionTree> * evidence : string list list
    | LeafNode of classification : string * evidence : string list list

//Extracts all of the classes in an example set
//@examples: The example set you want classes from
let getClasses (examples : string list list) (index : Map<string,int>) =
    List.map (fun (l : string list) -> l.[index.["class"]]) examples

let getClassValues (examples : string list list) (index : Map<string,int>) =
    let classes = getClasses examples index in 
    List.distinct classes

//Finds the most common class in an example set
//@examples: The example set you want classes from
let plurality (examples : string list list) (index : Map<string,int>) =
    let classes = getClasses examples index
    let majorityList = List.countBy (fun s -> s) classes |> List.sortBy (fun (_, y) -> -y)
    fst majorityList.Head

//Checks to see if all classes in remaining rows are the same
//@examples: The example set you want to check
let sameClassification (examples : string list list) (index : Map<string,int>) =
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
let percentAttributesPositive (attribute : string) (value : string) (examples : string list list) (index : Map<string,int>) (classes : string list) = 
    // First line calculates pk + nk, second line pk
    let examplesWithValue = List.filter (fun (x : string list) -> x.[index.[attribute]] = value) examples in
    let positiveValues = List.filter (fun (x : string list) -> x.[index.["class"]] = classes.Head) examplesWithValue in
    // If statement to avoid division by 0 errors if there are no examples for a given value
    if examplesWithValue.Length = 0 then 0.0 else (float) positiveValues.Length / (float) examplesWithValue.Length

//Total percent of examples that are positive (p / p + n)
//@examples: The rows in your data
let percentPositive (examples : string list list) (index : Map<string,int>) (classes : string list) =
    // First line calculates p (positive values)
    let positives = List.filter (fun (x : string list) -> x.[index.["class"]] = classes.Head) examples in
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
let remainder (attribute : string) (attributes : Map<string,string list>) (examples : string list list) (index : Map<string,int>) (classes : string list) =
    let values = attributes.[attribute] in
    // This whole list shenanigans does the sum of (pk + nk / p + n) * entropy(pk / pk + nk)
    List.map (fun (value : string) ->
        (percentValue attribute value examples index) * (entropy (percentAttributesPositive attribute value examples index classes))) values |>
        List.sum

//Calculates the information gain of choosing a particular attribute
//@attribute: The attribute to calculate information gain for
//@attributes: List of attributes
//@examples: Rows in the data
let infoGain (attribute : string) (attributes : Map<string,string list>) (examples : string list list) (index : Map<string,int>) (classes : string list) =
    (entropy (percentPositive examples index classes)) - (remainder attribute attributes examples index classes)

//Selects the attribute with the highest information gain
//@attributes: List of attributes
//@examples: Rows in the data
let mostImportant (attributes : Map<string,string list>) (examples : string list list) (index : Map<string,int>) (classes : string list) : string =
   // Get attributes in list form, calculate their gains, zip them together, sort, and pick the best one
   let attributeList = Map.toList attributes |> List.map (fun x -> fst x) in
   let gains = List.map (fun (attribute : string) -> infoGain attribute attributes examples index classes) attributeList in
   let zipped = List.zip attributeList gains |> List.sortBy (fun (_, y) -> -y) in
   fst zipped.Head
 
//Pure black wizard voodoo magic. A mutually recursive function that uses functions above to construct the tree itself
//@examples: Rows of data
//@attributes: List of attributes (backwards from rest of functions but I'm scared to change until committed)
//@parentexamples: The examples from the parent tree
let rec createSubtree (examples : string list list) (attributes : Map<string, string list>) (index : Map<string,int>) (classes : string list) (depth : int) : DecisionTree = 
    let attribute = mostImportant attributes examples index classes in // The attribute we're selecting to split on
    let values = attributes.[attribute] in // List of values for the attribute
    // Creates a list, that has one list for each value, which is a list of rows (themselves lists) that contain the given value
    let exs = List.map (fun value -> List.filter (fun (row : string list) ->
        row.[index.[attribute]] = value) examples) values in
    // A new attribute map, without the attribute we're splitting on
    let fewerAttributes = Map.remove attribute attributes in
    // Create a list of subtrees (by calling dtl) with the values created above
    let subtrees = List.map (fun newexs -> dtlHelper newexs fewerAttributes examples index (depth-1)) exs in
    // Match the values with their subtree in a list of tuples
    let zipped = List.zip values subtrees in
    // Convert the list of tuples into a map, and make create the InnerNode for the split
    InnerNode(attribute,Map.ofList(zipped),examples)

// HOW TO CALL : dtl examples attributes [] indexMap
and dtlHelper (examples : string list list) (attributes : Map<string,string list>) (parentExamples : string list list) (index : Map<string,int>) (depth : int) =
    // This is boring, but handles the end cases where we run out of examples, attributes, or have a unanimous node.
    let classes = getClassValues examples index in
    if examples.IsEmpty then LeafNode ((plurality parentExamples index),examples)
    elif depth = 0 then LeafNode ((plurality examples index),examples)
    elif sameClassification examples index then LeafNode ((examples.Head.[index.["class"]]),examples)
    elif Map.isEmpty attributes then LeafNode ((plurality examples index),examples)
    else createSubtree examples attributes index classes depth

// Everything between here and the END comment is just finding values for
// calculating the Chi-squared value. Everything after the end comment is 
// me trying to figure out the best way to actually do the pruning
// ****************** START Chi-squared calculations **************************

//Counts the number of positives in an example set (p)
//@examples: The example set given
//@index: The index mappings
//@classes: The possible class values
let positiveCount (examples : string list list) (index : Map<string,int>) (classes : string list) =
    let positives = List.filter (fun (x : string list) -> x.[index.["class"]] = classes.Head) examples in
    positives.Length

//Counts the number of negatives in an example set (n)
//@examples: The example set given
//@index: The index mappings
//@classes: The possible class values
// p
let negativeCount (examples : string list list) (index : Map<string,int>) (classes : string list) =
    let lengthPositive = positiveCount examples index classes in
    examples.Length - lengthPositive

//Calculates the expected positives : pk-hat (p * (pk + nk / p + n))
//@attribute: The attribute that has been split on
//@value: The value this split was given
//@examples: Example set given
//@index: Index map
//@classes: Possible class values
let expectedPositives (attribute : string) (value : string) (examples : string list list) (index : Map<string,int>) (classes : string list) =
    let positives = positiveCount examples index classes in
    let percentAttribute = percentValue attribute value examples index in
    (float) positives * percentAttribute

//Calculates the expected negatives : nk-hat (n * (pk + nk / p + n))
//@attribute: The attribute that has been split on
//@value: The value this split was given
//@examples: Example set given
//@index: Index map
//@classes: Possible class values
let expectedNegatives (attribute : string) (value : string) (examples : string list list) (index : Map<string,int>) (classes : string list) =
    let negatives = negativeCount examples index classes in
    let percentAttribute = percentValue attribute value examples index in
    (float) negatives * percentAttribute

// Actual positive count for a value (pk)
//@attribute: The attribute split on
//@value: The value this split was given
//@examples: Example set given
//@index: Index map
//@classes: Possible class values
let positivesOfValue (attribute : string) (value : string) (examples : string list list) (index : Map<string,int>) (classes : string list) =
    let examplesWithValue = List.filter (fun (x : string list) -> x.[index.[attribute]] = value) examples in
    let positives = List.filter (fun (x : string list) -> x.[index.["class"]] = classes.Head) examplesWithValue
    (float) positives.Length

//Actual negative count for a value (nk)
//@attribute: The attribute split on
//@value: The value this split was given
//@examples: Example set given
//@index: Index map
//@classes: Possible class values
let negativesOfValue (attribute : string) (value : string) (examples : string list list) (index : Map<string,int>) (classes : string list) =
    let examplesWithValue = List.filter (fun (x : string list) -> x.[index.[attribute]] = value) examples in
    (float) examplesWithValue.Length - positivesOfValue attribute value examples index classes

//For deviation calculation: (pk - pk-hat)^2 / pk-hat
//@attribute: The attribute split on
//@value: The value this split was given
//@examples: Example set given
//@index: Index map
//@classes: Possible class values
let positiveChiValue (attribute : string) (value : string) (examples : string list list) (index : Map<string,int>) (classes : string list) =
    let actualPositives = positivesOfValue attribute value examples index classes in
    let expectPositives = expectedPositives attribute value examples index classes in
    ((actualPositives - expectPositives) ** 2.0) / expectPositives

//For deviation calculation: (nk - nk-hat)^2 / nk-hat
//@attribute: The attribute split on
//@value: The value this split was given
//@examples: Example set given
//@index: Index map
//@classes: Possible class values
let negativeChiValue (attribute : string) (value : string) (examples : string list list) (index : Map<string,int>) (classes : string list) =
    let actualNegatives = negativesOfValue attribute value examples index classes in
    let expectNegatives = expectedNegatives attribute value examples index classes in
    ((actualNegatives - expectNegatives) ** 2.0) / expectNegatives

//Sum of the positive and negative values above
//@attribute: The attribute split on
//@value: The value this split was given
//@examples: Example set given
//@index: Index map
//@classes: Possible class values
let deviationValue (attribute : string) (value : string) (examples : string list list) (index : Map<string,int>) (classes : string list) =
    let posValue = positiveChiValue attribute value examples index classes in
    let negValue = negativeChiValue attribute value examples index classes in
    posValue + negValue

//The delta value -- sum of all deviation values
//@attribute: The attribute split on
//@value: The value this split was given
//@examples: Example set given
//@index: Index map
//@classes: Possible class values
let delta (attribute : string) (attributes : Map<string,string list>) (examples : string list list) (index : Map<string,int>) (classes : string list) =
    let values = attributes.[attribute] in
    List.map (fun value -> deviationValue attribute value examples index classes) values |>
    List.sum

//******************** END Chi-squared calculations ***************************

// From here on, this is just my attempts (and functions needed) to try
// to figure out how to actually prune the tree. It's a huge mess, don't
// waste too much time looking into these

//Determines if a given node is a leaf node, true if yes
//@tree: The node to test
let isLeaf (tree : DecisionTree) =
    match tree with
    | LeafNode _ -> true
    | _ -> false

//Determines if a node has only leaf descendents, returns true if yes
//We need to try pruning all nodes where this is the case
//@tree: The node to test
let parentOfLeaves (tree : DecisionTree) =
    match tree with
    | LeafNode _ -> false
    | InnerNode (_, attributeMap, _) ->
    let nodes = Map.toList attributeMap |> List.map (fun (_,y) -> y) in
    if List.forall isLeaf nodes then true else false

//Converts the tree into a list of all trees (and subtrees)
//Convert map to list, get the trees only, append the tree to the recursed list, concat them all together
//Note to self: This function took like 3 hours to figure out, please don't break it, just make a new function
//@tree The tree to traverse
let allTrees (tree : DecisionTree) : DecisionTree list = 
    let rec helper (tree : DecisionTree) : DecisionTree list =
        match tree with
        | LeafNode _ -> []
        | InnerNode(_, attributeMap, _) ->
            Map.toList attributeMap |> List.map (fun (_,y) -> y) |>
            List.map (fun child -> child :: helper child) |> List.concat
    tree :: helper tree

//Turn a node that needs to be pruned into a leaf node with plurality deciding the class
//@tree: Node to turn into a leaf
//@index: Index mapping
let leafify (tree : DecisionTree) (index : Map<string,int>) : DecisionTree = 
    match tree with
    | LeafNode (classification, examples) -> LeafNode (classification, examples)
    | InnerNode (_, _, examples) -> LeafNode(plurality examples index, examples)

//Returns a list of all nodes that are parents of only leafs (the nodes we need to check)
//@tree: The tree we want to check
let pruneCandidates (tree : DecisionTree) : DecisionTree list =
    let trees = allTrees tree in
    List.filter parentOfLeaves trees

//Performs a chi-squared test on an individual node, turns it into a leaf if
//not significant, otherwise just return the tree
//@tree: The node to test
//@attributes: Attribute map
//@index: Index map
//@classes: Possible class values
let chiTest (tree : DecisionTree) (attributes : Map<string,string list>) (index : Map<string,int>) (classes : string list)  = 
        match tree with
        | LeafNode (a,b) -> LeafNode (a,b)
        | InnerNode (attribute, _, examples) -> 
            let deltaValue = delta attribute attributes examples index classes in
            let df = (float) attributes.[attribute].Length - 1.0 in
            let chi = ChiSquared.CDF(df,deltaValue) in
            if chi < 0.95 then leafify tree index else tree

//Turn a tuple list to a map. I'm pretty sure this is already a thing, so probably use that instead
//@pairs: List of tuples to turn into a map
//@attributeMap: The map to add the key value pairs into
let rec listToMap (pairs : (string * DecisionTree) list) (attributeMap : Map<string,DecisionTree>) =
    match pairs with
    | [] -> attributeMap
    | h :: t -> listToMap t (attributeMap.Add(h))

//Just converts a node's subtree map into a list of tuples
//@subtrees: The subtree map
let getSubtrees (subtrees: Map<string,DecisionTree>) : (string * DecisionTree) list =
    Map.toList subtrees

//Does the actual pruning, given a full decision tree
//@tree: The tree to prune
//@attributes: Attribute map
//@index: Index map
//@classes: Possible class values
let rec prune (tree : DecisionTree) (attributes : Map<string,string list>) (index : Map<string,int>) (classes : string list) : DecisionTree = 
    match tree with
    | LeafNode (classification, examples) -> LeafNode (classification,examples)
    | InnerNode (attribute, subtrees, examples) ->
        let children = getSubtrees subtrees in
        let maybePrune = List.filter (fun (_,y) -> parentOfLeaves y) children in
        let otherNodes = (Set.ofList children) - (Set.ofList maybePrune) |> Set.toList in
        let chiTested = List.map (fun (x,y) -> (x,chiTest y attributes index classes)) maybePrune in
        let allNodes = List.append otherNodes chiTested in
        let recurse = List.map (fun (x,y) -> (x,prune y attributes index classes)) allNodes in
        let newSubtrees = Map.ofList recurse in
        InnerNode(attribute, newSubtrees, examples)

//DTL wrapper that allows a true/false variable to account for pruning
//@examples: Examples for decision tree
//@attributes: Attribute map
//@parentExamples: initialize as [], provides parentExample storage for algorithm
//@index: Index map
//@pruning: Prune tree if true
//@depth: How deep the tree is allowed to go, -1 for no limit
let dtl (examples : string list list) (attributes : Map<string,string list>) (parentExamples : string list list) (index : Map<string,int>) (depth : int) (pruning : bool) =
    let tree = dtlHelper examples attributes parentExamples index depth in
    let classes = getClassValues examples index in
    if pruning then (prune tree attributes index classes) else tree

// Stolen from Mitziu's code because I've been going at this for hours and don't want to deal with imports and git right now

 //Classify Individual Row
//@node: Current Node in the decision Tree
//@row: Current row being classified
//@indexMap: Map that maps attribute names to Index
let rec classify (node:DecisionTree) (row:string list) (indexMap: Map<string, int>): string=
    match node with
    | LeafNode (decision,_) -> decision
    | InnerNode (attribute, attributeMap, _) -> 
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
    let result = List.map2 (fun a b -> a = b) testDataResults groundTruth
    let accuracy = float (result |> List.sumBy (fun x -> if x = true then 1 else 0)) / float (List.length result)
    accuracy

//Dumps the Decision Tree object into JSON file
//@filePath: File path of output JSON file
//@root: Root of decision tree to be serialized
let dumpToJson (root:DecisionTree) (filePath:string) : string =
    let JSon_Content = Json.serialize root
    File.WriteAllText(filePath, JSon_Content) |> ignore
    JSon_Content

//Builds Decision Tree from the file content
//@filePath: File path to input JSON file
let buildFromJson (filePath:string) : DecisionTree =
    let json = File.ReadAllText(filePath)
    Json.deserialize<DecisionTree> json


//Create K_Folds
//@data: Entire data read from CSV file
//@k: How many folds there will be
let create_k_folds (data: 'a list) (k: int) : (('a list list * 'a list) list) = 
    //Shuffles the data
    //@data: Entire data read from CSV file
    let shuffle (data: 'a list) : ('a list) = 
        let swap_items (a: int) (b: int) (r:'a array) : unit =
            let temp = r.[a]
            r.[a] <- r.[b]
            r.[b] <- temp
        let rng = System.Random()
        let data' = Array.ofList data
        Array.iteri (fun i x -> swap_items i (rng.Next (Array.length data'))  data') data'
        data' |> List.ofArray

    //Splits into k different sublists
    //@data: Entire data read from CSV file
    //@k: How many folds there will be.
    let split_into_k (data: 'a list) (k: int) : ('a list list) = 
        let k = List.length data / k
        let cuts = [0 .. k .. ((List.length data) - k)]
        cuts |> List.map (fun x -> data.[x .. x + (k - 1)])



    let create_all_possible_sets (folds: 'a list list) (k: int) = 
        //Splits into train and test set
        //@folds: List of folds
        //@index: Which index should be used for test
        let create_train_test (folds: 'a list list) (index: int) =
            //Removes at specific index
            //@folds: List of folds
            //@index: Which index needs to removed
            let rec removeAtIndex (folds: 'a list list) (index:int) = 
                match folds with
                | h::t when index = 0 -> t
                | h::t -> h :: removeAtIndex t (index - 1)
                | _ -> []
            //Creates (train,test) set
            (removeAtIndex folds index, folds.[index])

        List.map (fun i -> create_train_test folds i) [0 .. k - 1]
        

    let shuffled_data = shuffle data
    let folds = split_into_k shuffled_data k
    create_all_possible_sets folds k

//Will Do K-Fold Validation on data set, returns average accuracy
//@data: data read from CSV file
//@k: How many folds
//@attributes: Attributes map read from JSON
//@indexMap: Index map read from JSON
//@pruning: Xi^2 pruning
//@depth: Depth to be used for pruning
let k_fold_validation (data: string list list) (k: int) (attributes: Map<string, string list>) (indexMap: Map<string, int>) (pruning : bool) (depth: int) : (float * float) = 
    let train_test_sets = create_k_folds data k
    let train_test_sets = train_test_sets |> List.map (fun (tr,te) -> (List.concat tr, te))    
    let validation_results = train_test_sets |> List.map (fun (train, test) -> testAccuracy test (dtl train attributes [] indexMap depth pruning) indexMap)
    let train_results = train_test_sets |> List.map(fun (train, test) -> testAccuracy train (dtl train attributes [] indexMap depth pruning) indexMap)

    ((1.0 - List.average train_results), (1.0 - List.average validation_results))

//Returns the model with the lowest validation error
//@data: data read from CSV file
//@k: How many folds
//@attributes: Attributes map read from JSON
//@indexMap: Index map read from JSON
let find_best_model (data: string list list) (k: int) (attributes: Map<string, string list>) (indexMap: Map<string, int>) =
    let mutable depth = 1
    let mutable test_results = []
    let mutable converge = false
    //Finds point where training error converges
    //Stops while loop when train error reaches 0.0
    while (converge = false) do
        let scores = k_fold_validation data k attributes indexMap false depth
        match scores with
        | (train, test) when train = 0.0 -> 
            test_results <- [test] |> List.append test_results
            depth <- depth + 1
            converge <- true
        | (train, test) -> 
            test_results <- [test] |> List.append test_results
            depth <- depth + 1
    
    //Finds best depth and returns a model built with that parameter
    let best_depth = test_results |> Seq.mapi (fun index value -> index, value) |> Seq.minBy snd
    match best_depth with
        | (index, value) -> dtl data attributes [] indexMap (index + 1) false

    
    //@depth: How deep the tree is allowed to go, -1 for no limit
    //let dtl (examples : string list list) (attributes : Map<string,string list>) (parentExamples : string list list) (index : Map<string,int>) (depth : int) (pruning : bool) =


(* Commented out because already another main in Program.fs
[<EntryPoint>]
let main argv = 
    //Example on how to run it
    (*printfn "%A" "This is a test!"
    let examples = mockExamples
    let mockTree = LeafNode("a")
    let mockRoot = InnerNode("test", Map.empty.Add("testValue", mockTree))
    printf "%s" (dumpToJson mockTree "testPath.txt")
    printf "%s" (dumpToJson mockRoot "testPath.txt")
    let jsonContent = dumpToJson mockRoot
    let rebuildTree = buildFromJson "testPath.txt"
    match rebuildTree with
        | InnerNode (s, m) -> printfn "Inner Node %s" s
        | LeafNode s -> printfn "Leaf Node %s" s
    *)

    let x = System.Console.ReadLine()
    0 // return an integer exit code
*)


// Testing examples
(*
let tennisTree = dtl tennisExamples tennisAttributes [] tennisIndex -1 true

let treeToJson = Json.serialize tennisTree
let jsonToTree = Json.deserialize<DecisionTree> treeToJson
*)
