module Rope

type Rope =
    | Node of int * int * Rope * Rope
    | Value of string
    | Empty

// Rope -> bool
// Determines whether the given Rope is balanced or not.
let isRopeUnbalanced (r : Rope) =
    match r with
        | Node (leftSz, rightSz, _, _) -> (leftSz * 2) < rightSz || (rightSz * 2) < leftSz
        | Value _ -> false
        | Empty -> false
    
// Rope -> Rope
// Performs the actual act of balancing a Rope. Takes an unbalanced Rope and returns a balanced one.
let performBalance (r : Rope) =
    match r with
        | Node (aSz, _, a, Node (bSz, _, b, Node (cSz, dSz, c, d)))
            | Node (aSz, _, a, Node (_, dSz, Node (bSz, cSz, b, c), d))
            | Node (_, dSz, Node (_, cSz, Node (aSz, bSz, a, b), c), d)
            | Node (_, dSz, Node (aSz, _, a, Node (bSz, cSz, b, c)), d)
            -> Node ((aSz + bSz), (cSz + dSz), Node (aSz, bSz, a, b), Node (cSz, dSz, c, d))
        | _ -> r

// Rope -> Rope
// Takes a Rope which may or may not be balanced. Guaranteed to return a balanced Rope.
let balance r =
    if isRopeUnbalanced r then performBalance r
    else r

// Rope -> int -> string -> Rope
// Inserts a value into a Rope at a position and returns the corresponding Rope. The returned Rope is guaranteed to be
// balanced.
let rec insert r pos value =
    match r with
        | Node (leftSz, rightSz, left, right) when pos < leftSz
            -> Node ((leftSz + 1), rightSz, insert left pos value, right)
                |> balance
        | Node (leftSz, rightSz, left, right) when pos >= leftSz
            -> Node (leftSz, rightSz + 1, left, insert right (pos - leftSz) value)
                |> balance
        | Value oldValue when pos >= 1
            -> Node (1, 1, Value oldValue, Value value)
        | Value oldValue when pos < 1
            -> Node (1, 1, Value value, Value oldValue)
        | _ -> Value value

// Rope -> string -> Rope
// Inserts a value into a Rope at the final position.
let rec insertEnd r value =
    match r with
        | Node (leftSz, rightSz, left, right)
            -> Node (leftSz, rightSz + 1, left, insertEnd right value)
                |> balance
        | Value oldValue -> Node (1, 1, Value oldValue, Value value)
        | _ -> Value value

// Rope -> int -> Rope
// Removes a value found at the given position from the given rope.
let rec remove r pos =
    match r with
        | Node (1, _, _, right) when pos = 0
            -> right
        | Node (_, 1, left, _) when pos >= 1
            -> left
        | Node (leftSz, rightSz, left, right) when pos < leftSz
            -> Node (leftSz - 1, rightSz, remove left pos, right)
                |> balance
        | Node (leftSz, rightSz, left, right) when pos >= leftSz
            -> Node (leftSz, rightSz - 1, left, remove right (pos - leftSz))
                |> balance
        | Value _ -> Empty
        | _ -> Empty
