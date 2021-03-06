/// try the zip function from the standard library
List.zip [ 1 .. 5 ] [ 5 .. -1 .. 1 ]

/// implement our own version
let rec myZip xs ys =
  match xs, ys with
  | [], [] -> []
  | x :: xs', y :: ys' -> (x, y) :: myZip xs' ys'
  | _, _ -> []

/// implement zipWith
let rec myZipWith f xs ys =
  match xs, ys with
  | [], [] -> []
  | x :: xs', y :: ys' -> (f x y) :: myZipWith f xs' ys'
  | _, _ -> []

/// using our new functions
myZip [ 1 .. 5 ] [ 5 .. -1 .. 1 ]

myZip [ 1 .. 5 ] [ 5 .. -1 .. -500 ]

let multiplier a b = a * b

myZipWith multiplier [ 1 .. 5 ] [ 1 .. 5 ]

myZipWith (*) [ 1 .. 5 ] [ 2 .. 6 ]

// oops... blows up with stack overflow
// myZipWith (+) [ 1 .. 1_000_000 ] [ 1 .. 1_000_000 ]

/// let's fix it by making it tail recursive
let myTailRecZipWith f xs ys =
  let rec loop acc xs ys =
    match xs, ys with
    | [], [] -> acc
    | x :: xs, y :: ys -> loop ((f x y) :: acc) xs ys
    | _, _ -> acc

  loop [] xs ys |> List.rev

/// this one works
let byTailRec = myTailRecZipWith (+) [ 1 .. 1_000_000 ] [ 1 .. 1_000_000 ]

/// another kind of optimization using a continuation
/// see also `foldBack` from our `MyList` module
(*
  For this example:
  let xs = 1 :: 2 :: []
  let ys = 10 :: 20 :: []
  let f = (+)

  // first step
  fun acc -> f 1 10 :: acc
  |> id  // this is the initial value of `cont`

  // which reduces to
  fun acc -> 11 :: acc |> id

  // second step
  fun acc -> f 2 20 :: acc
  |> fun acc -> 11 :: acc  // this and the next line is the next value of `cont`
  |> id

  // which reduces to
  fun acc -> 22 :: acc
  |> fun acc -> 11 :: acc  // this and the next line is the next value of `cont`
  |> id

  // base case
  []
  |> fun acc -> 22 :: acc  // this and the next two lines is the final value of `cont`
  |> fun acc -> 11 :: acc
  |> id
*)
let myContZipWith f xs ys =
  let rec loop xs ys cont =
    match xs, ys with
    | [], [] -> cont []
    | x :: xs, y :: ys -> loop xs ys (fun acc -> (f x y) :: acc |> cont)
    | _, _ -> cont []

  loop xs ys id

myContZipWith (+) [1;2] [10;20] // from the example comment above

/// this one works
let byCont = myContZipWith (+) [ 1 .. 1_000_000 ] [ 1 .. 1_000_000 ]

/// both optimization produce the same result
byTailRec = byCont //true

/// trying out the standard fold function
List.fold (+) 0 [ 1 .. 100 ]

let folder acc x = acc + " " + string x
List.fold folder "" [ 1 .. 5 ]

/// reimplementing myZip using foldBack2 from the standard library
let myZipFold2 xs ys =
  List.foldBack2 (fun x y acc -> (x, y) :: acc) xs ys []

myZipFold2 [ 0 .. 10 ] [ 1 .. 11 ]

/// reimplementing myZipWith using foldBack2 from the standard library
let myZipWithFold2 f xs ys =
  List.foldBack2 (fun x y acc -> (f x y) :: acc) xs ys []

myZipWithFold2 (+) [ 0 .. 10 ] [ 1 .. 11 ]
