open System.Globalization

let mockAttributes = Map.empty.
                        Add("outlook",["sunny";"overcast";"rainy"]).
                        Add("temp",["hot";"mild";"cold"]).
                        Add("humidity",["high";"normal"]).
                        Add("windy",["false";"true"])

let test = mockAttributes.["outlook"]

let mockExamples = [["yes";"sunny";"hot";"high";"false"]; // currently guessing that we put classification at index 0 
                    ["yes";"sunny";"hot";"high";"true"]; // this would always be consistent no matter size of data
                    ["yes";"overcast";"hot";"high";"false"];
                    ["yes";"rainy";"mild";"high";"false"]]

type mockTree =
    | Node of string * mockTree
    | Leaf of string

// CountBy counts number of occurances by each key, sort by sorts by 2nd element (descending)
let asdf = [1;2;3;4;4;4;2;2;2] |> List.countBy (fun s -> s) |> List.sortBy (fun (_, y) -> -y)
// fst is a function that lets you access the first element of a tuple
fst asdf.Head


let getClasses (examples : 'a list list) =
    List.map (fun (l : 'a list) -> l.Head) examples

let plurality (examples : 'a list list) =
    let classes = getClasses examples
    let majorityList = List.countBy (fun s -> s) classes |> List.sortBy (fun (_, y) -> -y)
    fst majorityList.Head

let sameClassification (examples : 'a list list) =
    let classes = getClasses examples
    List.forall (fun x -> x = classes.Head) classes

let dtl (examples : string list list) (attributes : Map<string,string list>) (parentExamples : string list list) =
    if examples.IsEmpty then Leaf (plurality parentExamples)
    elif sameClassification examples then Leaf (examples.Head.Head)
    elif Map.isEmpty attributes then Leaf (plurality parentExamples)
    else Leaf ""