module DocumentTest 

open System
open Xunit
open Document

let emptyDocument = { Data = Rope.Empty 
                      IsModified = false
                      Length = 0 }

let abDocument = { Data = Rope.fromList [ "a" ; "b" ]
                   IsModified = true
                   Length = 2 }

// Document -> String
// Overrides Document.toString to set the line ending to "$"
let toTestString (doc : Document) =
    toString "$" doc

////////////////////////////////////////////////////////////////////////////////
// toString tests
////////////////////////////////////////////////////////////////////////////////

[<Fact>]
let ``toString on a blank document produces a blank string`` () =
    Assert.Equal("", toTestString emptyDocument)

[<Fact>]
let ``toString on ab document produces document with a and b on two lines`` () =
    Assert.Equal("a$b", toTestString abDocument)

////////////////////////////////////////////////////////////////////////////////
// insert tests
////////////////////////////////////////////////////////////////////////////////

[<Fact>]
let ``Insert on a blank document gives us just one line`` () =
    Assert.Equal("a",
                 emptyDocument
                 |> insert 1 "a"
                 |> toTestString)

[<Fact>]
let ``Insert chained gives us multiple lines`` () =
    Assert.Equal("a$b$c$d$e",
                 emptyDocument
                 |> insert 1 "a"
                 |> insert 2 "b"
                 |> insert 3 "c"
                 |> insert 4 "d"
                 |> insert 5 "e"
                 |> toTestString)

[<Fact>]
let ``Insert on an existing line num places the new value before the previous value`` () =
    Assert.Equal("c$a$b",
                 emptyDocument
                 |> insert 1 "a"
                 |> insert 2 "b"
                 |> insert 1 "c"
                 |> toTestString)

[<Fact>]
let ``Insert updates isModified flag`` () =
    Assert.False(emptyDocument.IsModified)
    Assert.True(emptyDocument
                |> insert 1 "a"
                |> fun doc -> doc.IsModified)

[<Fact>]
let ``Insert updates Length`` () =
    Assert.Equal(0, emptyDocument.Length)
    Assert.Equal(1, emptyDocument |> insert 1 "a" |> fun d -> d.Length)
    Assert.Equal(5,
                 emptyDocument
                 |> insert 1 "a"
                 |> insert 2 "b"
                 |> insert 3 "c"
                 |> insert 4 "d"
                 |> insert 5 "e"
                 |> fun d -> d.Length)
                 
////////////////////////////////////////////////////////////////////////////////
// append tests
////////////////////////////////////////////////////////////////////////////////

[<Fact>]
let ``Append on an empty document replaces contents`` () =
    Assert.Equal("a", emptyDocument |> append 1 "a" |> toTestString)

[<Fact>]
let ``Append in succession creates multiple consecutive lines`` () =
    Assert.Equal("a$b$c$d$e",
                 emptyDocument
                 |> append 1 "a"
                 |> append 2 "b"
                 |> append 3 "c"
                 |> append 4 "d"
                 |> append 5 "e"
                 |> toTestString)

[<Fact>]
let ``Append on existing line places new value after old value`` () =
    Assert.Equal("a$c$b",
                 emptyDocument
                 |> append 1 "a"
                 |> append 2 "b"
                 |> append 1 "c"
                 |> toTestString)

[<Fact>]
let ``Append sets IsModified flag`` () =
    Assert.False(emptyDocument.IsModified)
    Assert.True(emptyDocument
                |> append 1 "a"
                |> fun d -> d.IsModified)

[<Fact>]
let ``Append updates Length`` () =
    Assert.Equal(0, emptyDocument.Length)
    Assert.Equal(2,
                 emptyDocument
                 |> append 1 "a"
                 |> append 2 "b"
                 |> fun d -> d.Length) 
