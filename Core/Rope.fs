module Rope

type Rope =
    | Node of int * int * Rope * Rope
    | Value of string
    | Empty

// Rope -> int
// Gets the size of the given rope
let size (r : Rope) =
    match r with
        | Node (leftSz, rightSz, _, _) -> leftSz + rightSz
        | _ -> 1

// Rope -> Rope -> Rope
// Creates a node with the given left and right side.
let makeNode (left : Rope) (right : Rope) =
    Node ((size left), (size right), left, right)

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

// int -> string list -> Rope -> Rope
// Inserts all the values given into a rope at the specified position.
let rec insertAll (pos : int) (values : string list) (r : Rope) =
    match values with
        | first :: rest ->
            insert r pos first
            |> insertAll (pos + 1) rest
        | [] -> r

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

// int -> int -> Rope -> Rope
// Removes 'count' items from the given rope starting at 'pos'.
let rec removeAll (pos : int) (count : int) (r : Rope) =
    if count >= 0 then
        remove r pos
        |> removeAll pos (count - 1)
    else
        r

// Rope -> int -> string -> Rope
// Changes the value at the position specified in the rope to a new value provided.
let rec change (r : Rope) (pos : int) (newValue : string) =
    match r with
        | Node (leftSz, rightSz, left, right) when pos < leftSz
            -> Node (leftSz, rightSz, change left pos newValue, right)
        | Node (leftSz, rightSz, left, right) when pos >= leftSz
            -> Node (leftSz, rightSz, left, change right (pos - leftSz) newValue)
        | Value _ -> Value newValue
        | _ -> Empty

// int -> Rope -> string
// Gets the Value at the given position.
let rec get (pos : int) (r : Rope) =
    match r with
        | Node (leftSz, _, left, _) when pos < leftSz ->
            get pos left
        | Node (leftSz, _, _, right) when pos >= leftSz ->
            get (pos - leftSz) right
        | Value v ->
            v
        | _ -> ""

// int -> int -> Rope -> string list -> string list
// Gets all values from the start position to the end position and returns a list of those
// values.
let rec private getAllHelper (start : int) (ending : int) (r : Rope) (acc : string list) =
    if ending < start then
        acc
    else
        get ending r :: acc
        |> getAllHelper start (ending - 1) r

// int -> int -> Rope -> string list
// Gets all values from the start position to the end position and returns a list of those
// values.
let getAll (start : int) (ending : int) (r : Rope) =
    getAllHelper start ending r []

// Rope -> string list -> string list
let rec private toListHelper (r : Rope) (accumulator : string list) =
    match r with
        | Node (_, _, left, right) ->
            accumulator
            |> toListHelper right
            |> toListHelper left
        | Value v -> v :: accumulator
        | _ -> []

// Rope -> string list
// Converts a given Rope into a list of its values. Values are ordered by their
// position in the Rope.
let toList (r : Rope) =
    toListHelper r []

// string list -> Rope
// Takes a list of strings and returns a Rope which contains the strings in the positions they
// appeared in the list.
let fromList (lst : string list) =
    lst
    |> List.fold insertEnd Empty
            
            
