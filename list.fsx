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

  let reverse xs = fold (fun acc v -> v ^> acc) Empty xs

  let append xs ys =
    let rec loop xs =
      match xs with
      | Empty -> ys
      | Elem (v, xs') -> v ^> loop xs'

    loop xs

  let filter (f: 'a -> bool) (xs: 'a MyList) =
    fold (fun acc v -> if f v then v ^> acc else acc) Empty xs

  let map (f: 'a -> 'b) (xs: 'a MyList) =
    fold (fun acc v -> f v ^> acc) Empty xs |> reverse

  let toString xs =
    let s = fold (fun acc v -> acc + (if acc = "" then "" else " ^> ") + string v) "" xs
    if s = "" then "Empty" else s + " ^> Empty"

///use the module
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
