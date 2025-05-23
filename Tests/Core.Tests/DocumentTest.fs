module DocumentTest 

open System
open Xunit
open Document

let emptyDocument = Ok { Data = Rope.Empty 
                         IsModified = false
                         Length = 0 }

let abDocument = Ok { Data = Rope.fromList [ "a" ; "b" ]
                      IsModified = true
                      Length = 2 }

// Document -> String
// Overrides Document.toString to set the line ending to "$"
let toTestString (doc : Document) =
    toString "$" doc |> Ok

let (>>=) value expr =
    Result.bind expr value

let fail str =
    Assert.True(false, str)

////////////////////////////////////////////////////////////////////////////////
// toString tests
////////////////////////////////////////////////////////////////////////////////

[<Fact>]
let ``toString on a blank document produces a blank string`` () =
    emptyDocument
    >>= toTestString
    |> function
        | Ok s -> Assert.Equal("", s)
        | Error s -> fail s

[<Fact>]
let ``toString on ab document produces document with a and b on two lines`` () =
    abDocument
    >>= toTestString
    |> function
        | Ok s -> Assert.Equal("a$b", s)
        | Error s -> fail s

////////////////////////////////////////////////////////////////////////////////
// input validation tests
////////////////////////////////////////////////////////////////////////////////

[<Fact>]
let ``Line number 0 is valid on empty document`` =
    emptyDocument
    >>= insert 0 "a"
    |> function
        | Ok s -> Assert.True(true)
        | Error s -> Assert.True(false)

[<Fact>]
let ``Line number 0 is not valid on document with contents`` =
    emptyDocument
    >>= insert 0 "a"
    >>= insert 0 "b"
    |> function
        | Ok _ -> fail "Expected an error"
        | Error _ -> Assert.True(true)

[<Fact>]
let ``Line number <= our document length is accepted`` =
    emptyDocument
    >>= insert 0 "a"
    >>= insert 1 "b"
    |> function
        | Ok _ -> Assert.True(true)
        | Error _ -> Assert.True(false)

[<Fact>]
let ``Line number > our document length is rejected`` =
    emptyDocument
    >>= insert 100 "a"
    |> function
        | Ok _ -> fail "Expected an error"
        | Error _ -> Assert.True(true)
        
////////////////////////////////////////////////////////////////////////////////
// insert tests
////////////////////////////////////////////////////////////////////////////////

[<Fact>]
let ``Insert on a blank document gives us just one line`` () =
    emptyDocument
    >>= insert 0 "a"
    >>= toTestString
    |> function
        | Ok s -> Assert.Equal("a", s)
        | Error s -> fail s

[<Fact>]
let ``Insert on a line that doesn't exist errors out`` () =
    emptyDocument
    >>= insert 1 "a"
    |> function
        | Ok s -> Assert.True(false)
        | Error s -> Assert.True(true)

[<Fact>]
let ``Insert chained gives us multiple lines`` () =
    emptyDocument
    >>= insert 0 "c"
    >>= insert 1 "b"
    >>= insert 1 "a"
    >>= toTestString
    |> function
        | Ok s -> Assert.Equal("a$b$c", s)
        | Error s -> Assert.True(false, s)
       
[<Fact>]
let ``Insert updates isModified flag`` () =
    emptyDocument
    >>= insert 0 "a"
    |> function
        | Ok doc -> Assert.True(doc.IsModified)
        | Error s -> Assert.True(false, s)

[<Fact>]
let ``Insert updates Length`` () =
    emptyDocument
    >>= insert 0 "a"
    >>= insert 1 "b"
    >>= insert 1 "c"
    |> function
        | Ok doc -> Assert.Equal(3, doc.Length)
        | Error s -> fail s
                 
////////////////////////////////////////////////////////////////////////////////
// append tests
////////////////////////////////////////////////////////////////////////////////

[<Fact>]
let ``Append on an empty document replaces contents`` () =
    emptyDocument
    >>= append 0 "a"
    >>= toTestString
    |> function
        | Ok s -> Assert.Equal("a", s)
        | Error s -> fail s
    
[<Fact>]
let ``Append in succession creates multiple consecutive lines`` () =
    emptyDocument
    >>= append 0 "a"
    >>= append 1 "b"
    >>= append 2 "c"
    >>= toTestString
    |> function
        | Ok s -> Assert.Equal("a$b$c", s)
        | Error s -> fail s
        
[<Fact>]
let ``Append sets IsModified flag`` () =
     emptyDocument
    >>= append 0 "a"
    |> function
        | Ok doc -> Assert.True(doc.IsModified)
        | Error s -> fail s

[<Fact>]
    emptyDocument
    >>= append 0 "a"
    >>= append 1 "b"
    >>= append 1 "c"
    |> function
        | Ok doc -> Assert.Equal(3, doc.Length)
        | Error s -> fail s
