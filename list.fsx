//-------------------
// Create a list type in its own module

module MyList =
  type MyList<'t> =
    | Elem of 't * MyList<'t>
    | Empty

  let cons x xs = Elem(x, xs)
  let (^>) = cons

  let length xs =
    let rec loop acc xs =
      match xs with
      | Empty -> acc
      | Elem (_, xs) -> loop (acc + 1) xs

    loop 0 xs

  let rec fold (f: 'acc -> 'v -> 'acc) (acc: 'acc) (xs: 'v MyList) =
    match xs with
    | Empty -> acc
    | Elem (v, xs') -> fold f (f acc v) xs'

  // using continuation-based recursion so we won't blow the stack
  let foldBack (f: 'v -> 'acc -> 'acc) (xs: 'v MyList) (acc: 'acc) =
    let rec loop f xs cont =
      match xs with
      | Empty -> cont acc
      | Elem (v, xs') -> loop f xs' (fun acc -> f v acc |> cont)

    loop f xs id

  let reverse xs = fold (fun acc v -> v ^> acc) Empty xs

  let append xs ys =
    let rec loop xs =
      match xs with
      | Empty -> ys
      | Elem (v, xs') -> v ^> loop xs'

    loop xs

  let filter (f: 'a -> bool) (xs: 'a MyList) =
    fold (fun acc v -> if f v then v ^> acc else acc) Empty xs

  // if we want to keep the original order with fold, we need to reverse
  let map' (f: 'a -> 'b) (xs: 'a MyList) =
    fold (fun acc v -> f v ^> acc) Empty xs |> reverse

  // using foldBack, we can start from the right-most element and maintain order
  let map (f: 'a -> 'b) (xs: 'a MyList) =
    foldBack (fun v acc -> f v ^> acc) xs Empty

  let toString xs =
    foldBack (fun v acc -> string v + " ^> " + acc) xs ""
    + "Empty"

/// ==========================
/// use the module
/// send the entire file into the REPL and then `open MyList` and `open MyListTests` to run the code here

module MyListTests =
  open MyList

  let myList = 1 ^> 2 ^> 3 ^> 4 ^> 5 ^> Empty
  printfn $"myList length = {length myList}"
  fold (+) 0 myList
  fold (fun acc v -> acc + string v) "" myList
  filter (fun x -> x % 2 = 0) myList
  reverse myList

  append myList (9 ^> 10 ^> Empty)
  toString myList
  toString Empty
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
