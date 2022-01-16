//-------------------
// Create a list type in its own module

module MyList

type MyList<'t> =
  | Elem of 't * MyList<'t>
  | Empty

/// Add an element to a `MyList`
let cons x xs = Elem(x, xs)

/// `cons` as an operator
let (^>) = cons

/// Compute the length
let length xs =
  let rec loop acc xs =
    match xs with
    | Empty -> acc
    | Elem (_, xs) -> loop (acc + 1) xs

  loop 0 xs

/// Transform a `MyList` into another data structure represented by `acc`
/// The function `f` is applied from the left. Example for a 3-element `MyList`:
///
/// `f (f (f acc0 e1) e2) e3`
let rec fold (f: 'acc -> 'v -> 'acc) (acc: 'acc) (xs: 'v MyList) =
  match xs with
  | Empty -> acc
  | Elem (v, xs') -> fold f (f acc v) xs'

/// Transform a `MyList` into another data structure represented by `acc`
///
/// Folds from the right using continuation-based recursion so we won't blow the stack
///
/// The function `f` is applied from the left. Example for a 3-element `MyList`:
///
/// `f e1 (f e2 (f e3 acc0))`
let foldBack (f: 'v -> 'acc -> 'acc) (xs: 'v MyList) (acc: 'acc) =
  let rec loop f xs cont =
    match xs with
    | Empty -> cont acc
    | Elem (v, xs') -> loop f xs' (fun acc -> f v acc |> cont)

  loop f xs id

/// Reverse the order of `MyList`
let reverse xs = fold (fun acc v -> v ^> acc) Empty xs

/// Find the end of `xs`, then append `ys` to the end
let append xs ys =
  let rec loop xs =
    match xs with
    | Empty -> ys
    | Elem (v, xs') -> v ^> loop xs'

  loop xs

/// Selects elements of `MyList` for which the predicate function `f` returns `true`
let filter (f: 'a -> bool) (xs: 'a MyList) =
  fold (fun acc v -> if f v then v ^> acc else acc) Empty xs

/// If we want to keep the original order with `fold`, we need to `reverse`
let map' (f: 'a -> 'b) (xs: 'a MyList) =
  fold (fun acc v -> f v ^> acc) Empty xs |> reverse

/// Transform a `MyList` by applying `f` to every member of the list
/// Uses `foldBack` to start from the right-most element and maintain order
let map (f: 'a -> 'b) (xs: 'a MyList) =
  foldBack (fun v acc -> f v ^> acc) xs Empty

/// More compact string form showing our `cons` operator
let toString xs =
  foldBack (fun v acc -> string v + " ^> " + acc) xs ""
  + "Empty"
