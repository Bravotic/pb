module RopeTest 

open System
open Xunit

//   X
//  / \
// a   b
let balancedTree1 =
    Rope.Node (1, 1, Rope.Value "a", Rope.Value "b")

//      Y
//     / \
//    /   \
//   X     Z
//  / \   / \
// a   b c   d
let balancedTree2 =
    Rope.Node (2, 2,
        Rope.Node (1, 1,
            Rope.Value "a",
            Rope.Value "b"),
        Rope.Node (1, 1,
            Rope.Value "c",
            Rope.Value "d"))

//     X
//    / \
//   Y   b
//  / \
// c   a 
let balancedTree3 =
    Rope.Node (2, 1,
        Rope.Node (1, 1,
            Rope.Value "c",
            Rope.Value "a"),
        Rope.Value "b")

//   X
//  / \
// a   Y
//    / \
//   b   c
let balancedTree4 =
    Rope.Node (1, 2,
        Rope.Value "a",
        Rope.Node (1, 1,
            Rope.Value "b",
            Rope.Value "c"))

//   X
//  / \
// c   d
let balancedTree5 =
    Rope.Node (1, 1,
        Rope.Value "c",
        Rope.Value "d")
        
//   X
//  / \
// a   Y
//    / \
//   b   Z
//      / \
//     c   d
let unbalancedTree1 =
    Rope.Node (1, 3,
        Rope.Value "a",
        Rope.Node (1, 2,
            Rope.Value "b",
            Rope.Node (1, 1,
                Rope.Value "c",
                Rope.Value "d")))

//    X
//   / \
//  a   Y
//     / \
//    Z   d
//   / \
//  b   c
let unbalancedTree2 =
    Rope.Node (1, 3,
        Rope.Value "a",
        Rope.Node (2, 1,
            Rope.Node (1, 1,
                Rope.Value "b",
                Rope.Value "c"),
            Rope.Value "d"))

//     X
//    / \
//   Y   d
//  / \
// a   Z
//    / \
//   b   c
let unbalancedTree3 =
    Rope.Node (3, 1,
        Rope.Node (1, 2,
            Rope.Value "a",
            Rope.Node (1, 1,
                Rope.Value "b",
                Rope.Value "c")),
        Rope.Value "d")

//       X
//      / \
//     Y   d
//    / \
//   Z   c
//  / \
// a   b
let unbalancedTree4 =
    Rope.Node (3, 1,
        Rope.Node (2, 1,
            Rope.Node (1, 1,
                Rope.Value "a",
                Rope.Value "b"),
            Rope.Value "c"),
        Rope.Value "d")

////////////////////////////////////////////////////////////////////////////////
// isRopeBalanced tests
////////////////////////////////////////////////////////////////////////////////

[<Fact>]
let ``An empty rope is always balanced`` () =
    Assert.Equal(false, Rope.isRopeUnbalanced(Rope.Empty))

[<Fact>]
let ``A single value object is always balanced`` () =
    Assert.Equal(false, Rope.isRopeUnbalanced(Rope.Value "Hello"))

[<Fact>]
let ``Balanced Tree case 1 is balanced`` () =
    Assert.Equal(false, Rope.isRopeUnbalanced(balancedTree1))
    
[<Fact>]
let ``Balanced Tree case 2 is balanced`` () =
    Assert.Equal(false, Rope.isRopeUnbalanced(balancedTree2))

[<Fact>]
let ``Unbalanced Tree Case 1 is flagged as unbalanced`` () =
    Assert.Equal(true, Rope.isRopeUnbalanced(unbalancedTree1))

[<Fact>]
let ``Unbalanced Tree case 2 is flagged as unbalanced`` () =
    Assert.Equal(true, Rope.isRopeUnbalanced(unbalancedTree2))

[<Fact>]
let ``Unbalanced Tree case 3 is flagged as unbalanced`` () =
    Assert.Equal(true, Rope.isRopeUnbalanced(unbalancedTree3))

[<Fact>]
let ``Unbalanced Tree case 4 is flagged as unbalanced`` () =
    Assert.Equal(true, Rope.isRopeUnbalanced(unbalancedTree4))

////////////////////////////////////////////////////////////////////////////////
// balance tests
////////////////////////////////////////////////////////////////////////////////  

[<Fact>]
let ``Empty tree is returned when Empty is balanced`` () =
    Assert.Equal(Rope.Empty, Rope.balance Rope.Empty)

[<Fact>]
let ``Single Value is returned when Value is balanced`` () =
    Assert.Equal(Rope.Value "a", Rope.balance (Rope.Value "a"))

[<Fact>]
let ``Balanced Tree 1 is unchanged when balance is called`` () =
    Assert.Equal(balancedTree1, Rope.balance balancedTree1)

[<Fact>]
let ``Balanced Tree 2 is unchanged when balance is called`` () =
    Assert.Equal(balancedTree2, Rope.balance balancedTree2)

[<Fact>]
let ``Unbalanced Tree 1 is balanced into Balanced Tree 2`` () =
    Assert.Equal(balancedTree2, Rope.balance unbalancedTree1)

[<Fact>]
let ``Unbalanced Tree 2 is balanced into Balanced Tree 2`` () =
    Assert.Equal(balancedTree2, Rope.balance unbalancedTree2)

[<Fact>]
let ``Unbalanced Tree 3 is balanced into Balanced Tree 2`` () =
    Assert.Equal(balancedTree2, Rope.balance unbalancedTree3)

[<Fact>]
let ``Unbalanced Tree 4 is balanced into Balanced Tree 2`` () =
    Assert.Equal(balancedTree2, Rope.balance unbalancedTree4)

////////////////////////////////////////////////////////////////////////////////
// insert tests
////////////////////////////////////////////////////////////////////////////////

[<Fact>]
let ``Empty tree is replaced with single value on insert`` () =
    Assert.Equal(Rope.Value "a", Rope.insert Rope.Empty 1 "a")

[<Fact>]
let ``Value is replaced with Node if another value is inserted`` () =
    Assert.Equal(Rope.Node (1, 1, Rope.Value "a", Rope.Value "b"),
        Rope.insert (Rope.Value "a") 1 "b")

[<Fact>]
let ``Value is added to left of Node when pos is < 1`` () =
    Assert.Equal(Rope.Node (1, 1, Rope.Value "b", Rope.Value "a"),
        Rope.insert (Rope.Value "a") 0 "b")

[<Fact>]
let ``Value is added to right of Node when pos is >= 1`` () =
    Assert.Equal(Rope.Node (1, 1, Rope.Value "a", Rope.Value "b"),
       Rope.insert (Rope.Value "a") 1 "b")

[<Fact>]
let ``Value descends left in a Node when pos < left size`` () =
    Assert.Equal(balancedTree3, Rope.insert balancedTree1 0 "c")

[<Fact>]
let ``Value descends right in a Node when pos >= left size`` () =
    Assert.Equal(balancedTree4, Rope.insert balancedTree1 3 "c")

[<Fact>]
let ``When a left insert would cause an imbalance, balance is called`` () =
    Assert.Equal(Rope.balance unbalancedTree4,
        Rope.insert (Rope.insert balancedTree5 0 "b") 0 "a")
    
[<Fact>]
let ``When a right insert would cause an imbalance, balance is called`` () =
    Assert.Equal(Rope.balance unbalancedTree1, Rope.insert balancedTree4 4 "d")

////////////////////////////////////////////////////////////////////////////////
// remove tests
////////////////////////////////////////////////////////////////////////////////

[<Fact>]
let ``Remove on an Empty tree retuns Empty`` () =
    Assert.Equal(Rope.Empty, Rope.remove Rope.Empty 0)

[<Fact>]
let ``Remove on a single Value returns Empty`` () =
    Assert.Equal(Rope.Empty, Rope.remove (Rope.Value "a") 0)

[<Fact>]
let ``Remove with a pos >= 1 removes right node and returns left`` () =
    Assert.Equal(Rope.Value "a", Rope.remove balancedTree1 1)

[<Fact>]
let ``Remove with a pos of 0 removes left node and returns right`` () =
    Assert.Equal(Rope.Value "b", Rope.remove balancedTree1 0)

[<Fact>]
let ``Remove on a node descends right when pos >= left size`` () =
    Assert.Equal(balancedTree1, Rope.remove balancedTree4 3)

[<Fact>]
let ``Remove on a node descends left when pos < left size`` () =
    Assert.Equal(balancedTree1, Rope.remove balancedTree3 0)

////////////////////////////////////////////////////////////////////////////////
// change tests
////////////////////////////////////////////////////////////////////////////////

[<Fact>]
let ``Change on Empty rope returns Empty`` () =
    Assert.Equal(Rope.Empty, Rope.change Rope.Empty 0 "q")

[<Fact>]
let ``Change on Value returns the new Value`` () =
    Assert.Equal(Rope.Value "q", Rope.change (Rope.Value "a") 0 "q")

[<Fact>]
let ``Change on node descends left if pos < left size`` () =
    Assert.Equal(Rope.Node (1, 1, (Rope.Value "q"), (Rope.Value "b")),
        Rope.change balancedTree1 0 "q")

[<Fact>]
let ``Change on node descends right if pos >= left size`` () =
    Assert.Equal(Rope.Node (1, 1, (Rope.Value "a"), (Rope.Value "q")),
        Rope.change balancedTree1 1 "q")

////////////////////////////////////////////////////////////////////////////////
// toList tests
////////////////////////////////////////////////////////////////////////////////

[<Fact>]
let ``toList on an Empty rope produces an empty list`` () =
    Assert.Equal<Collections.Generic.IEnumerable<string>>([], Rope.toList Rope.Empty)

[<Fact>]
let ``toList on a single value produces a list with just that value`` () =
    Assert.Equal<Collections.Generic.IEnumerable<string>>([ "a" ], Rope.toList (Rope.Value "a"))

[<Fact>]
let ``toList on a Node concats the left and right side`` () =
    Assert.Equal<Collections.Generic.IEnumerable<string>>([ "a" ; "b" ], Rope.toList balancedTree1)
