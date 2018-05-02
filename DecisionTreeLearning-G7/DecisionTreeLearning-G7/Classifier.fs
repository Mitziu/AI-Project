let mockExamples = [["yes";"sunny";"hot";"high";"false"]; // currently guessing that we put classification at index 0 
                    ["yes";"sunny";"hot";"high";"true"]; // this would always be consistent no matter size of data
                    ["yes";"overcast";"hot";"high";"false"];
                    ["yes";"rainy";"mild";"high";"false"]]
                    //Decision, Outlook, ???, Humidity, Wind

let a = 'a'

let mockIndex = Map.empty.Add("outlook", 1).Add("temp",2).Add("humidity", 3).Add("wind", 4)


type DecisionTree = 
    | LeafNode of string
    | InnerNode of string * Map<string, DecisionTree>

//Mock Tree to test classification
// Building mock tree based on image found in 
//https://nullpointerexception1.wordpress.com/2017/12/16/a-tutorial-to-understand-decision-tree-id3-learning-algorithm/

let Yes = LeafNode("Yes")
let No = LeafNode("No")

let Humidity = InnerNode("humidity", Map.empty.Add("high", No).Add("normal", Yes))

let Wind = InnerNode("wind", Map.empty.Add("true", No).Add("false", Yes))

let Outlook = InnerNode("outlook", Map.empty.Add("sunny", Humidity).Add("overcast", Yes).Add("rainy", Wind))

let rec classify (node:DecisionTree) (row:string list) (indexMap: Map<string, int>): string=
    match node with
    | LeafNode decision -> decision
    | InnerNode (attribute, attributeMap) -> 
        let index = (Map.find attribute indexMap)
        let attributeValue = row.[index]
        let newNode = (Map.find attributeValue attributeMap)
        classify newNode row indexMap
        
let classifyAllRows (data:string list list) (root:DecisionTree) (indexMap: Map<string, int>): string list =
    data |> List.map (fun row -> classify root row indexMap)
