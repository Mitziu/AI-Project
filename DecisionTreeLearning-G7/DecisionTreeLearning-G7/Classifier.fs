let mockExamples = [["yes";"sunny";"hot";"high";"false"]; // currently guessing that we put classification at index 0 
                    ["yes";"sunny";"hot";"high";"true"]; // this would always be consistent no matter size of data
                    ["yes";"overcast";"hot";"high";"false"];
                    ["yes";"rainy";"mild";"high";"false"]]
                    //Decision, Outlook, ???, Humidity, Wind

let a = 'a'

let mockIndex = Map.empty.Add("outlook", 1).Add("temp",2).Add("humidity", 3).Add("wind", 4)

//Decision Tree
type DecisionTree = 
    | LeafNode of string
    | InnerNode of string * Map<string, DecisionTree>

//Mock Tree to test classification
// Building mock tree based on image found in 
//https://nullpointerexception1.wordpress.com/2017/12/16/a-tutorial-to-understand-decision-tree-id3-learning-algorithm/

let Yes = LeafNode("yes")
let No = LeafNode("no")

let Humidity = InnerNode("humidity", Map.empty.Add("high", No).Add("normal", Yes))

let Wind = InnerNode("wind", Map.empty.Add("true", No).Add("false", Yes))

let Outlook = InnerNode("outlook", Map.empty.Add("sunny", Humidity).Add("overcast", Yes).Add("rainy", Wind))

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
    let groundTruth = testData |> List.map (fun row -> row.[0])
    printfn "%A" testDataResults
    printfn "%A" groundTruth
    let result = List.map2 (fun a b -> a = b) testDataResults groundTruth
    let accuracy = float (result |> List.sumBy (fun x -> if x = true then 1 else 0)) / float (List.length result)
    accuracy