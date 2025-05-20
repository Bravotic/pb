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
