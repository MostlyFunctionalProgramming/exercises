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
myTailRecZipWith (+) [ 1 .. 1_000_000 ] [ 1 .. 1_000_000 ]

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
