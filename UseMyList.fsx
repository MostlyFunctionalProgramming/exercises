/// ==========================
/// use the MyList module

#load "MyList.fsx"

open MyList

let myList = 1 ^> 2 ^> 3 ^> 4 ^> 5 ^> Empty
printfn $"myList length = {length myList}"
fold (+) 0 myList
fold (fun acc v -> acc + string v) "" myList
filter (fun x -> x % 2 = 0) myList
reverse myList

append myList (9 ^> 10 ^> Empty)
// toString myList
// toString Empty
map ((*) 3) myList

fold (fun acc v -> v :: acc) [] myList
foldBack (fun v acc -> v :: acc) myList []

//create a MyList of 1M elements and sum them using both versions of fold
let myBigList =
  List.fold (fun acc v -> v ^> acc) Empty [ 1UL .. 1_000_000UL ]

myBigList |> fun xs -> foldBack (+) xs 0UL
myBigList |> fold (+) 0UL

///=========================
// experiments folding a list into a record type

open System

type MyRec = { myMin: int; myMax: int }

let findMinMax xs =
  let rec loop acc xs =
    match xs with
    | Empty -> acc
    | Elem (x, xs) ->
      loop
        { myMin = min x acc.myMin
          myMax = max x acc.myMax }
        xs

  match xs with
  | Empty -> None
  | xs ->
    loop
      { myMin = Int32.MaxValue
        myMax = Int32.MinValue }
      xs
    |> Some

// try our new function `findMinMax`
let testList = (0 ^> -8 ^> 100 ^> 10 ^> Empty)

findMinMax testList

// can we implement findMinMax with the fold we defined for MyList?
let minMaxFolder acc v =
  match acc with
  | Some acc ->
    Some
      { myMin = min v acc.myMin
        myMax = max v acc.myMax }
  | None -> Some { myMin = v; myMax = v }

fold minMaxFolder None testList
