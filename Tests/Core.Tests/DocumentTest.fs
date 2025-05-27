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
    >>= insert 0 0 [ "a" ]
    |> function
        | Ok s -> Assert.True(true)
        | Error s -> Assert.True(false)

[<Fact>]
let ``Line number 0 is not valid on document with contents`` =
    emptyDocument
    >>= insert 0 0 [ "a" ]
    >>= insert 0 0 [ "b" ]
    |> function
        | Ok _ -> fail "Expected an error"
        | Error _ -> Assert.True(true)

[<Fact>]
let ``Line number <= our document length is accepted`` =
    emptyDocument
    >>= insert 0 0 [ "a" ]
    >>= insert 1 1 [ "b" ]
    |> function
        | Ok _ -> Assert.True(true)
        | Error _ -> Assert.True(false)

[<Fact>]
let ``Line number > our document length is rejected`` =
    emptyDocument
    >>= insert 100 100 [ "a" ]
    |> function
        | Ok _ -> fail "Expected an error"
        | Error _ -> Assert.True(true)
        
////////////////////////////////////////////////////////////////////////////////
// insert tests
////////////////////////////////////////////////////////////////////////////////

[<Fact>]
let ``Insert on a blank document gives us just one line`` () =
    emptyDocument
    >>= insert 0 0 [ "a" ]
    >>= toTestString
    |> function
        | Ok s -> Assert.Equal("a", s)
        | Error s -> fail s

[<Fact>]
let ``Insert on a line that doesn't exist errors out`` () =
    emptyDocument
    >>= insert 1 1 [ "a" ]
    |> function
        | Ok s -> Assert.True(false)
        | Error s -> Assert.True(true)

[<Fact>]
let ``Insert chained gives us multiple lines`` () =
    emptyDocument
    >>= insert 0 0 [ "c" ]
    >>= insert 1 1 [ "b" ]
    >>= insert 1 1 [ "a" ]
    >>= toTestString
    |> function
        | Ok s -> Assert.Equal("a$b$c", s)
        | Error s -> Assert.True(false, s)

[<Fact>]
let ``Chained calls are equivalent to one call with multiple values`` () =
    let chained =
        emptyDocument
        >>= insert 0 0 [ "d" ]
        >>= insert 1 1 [ "c" ]
        >>= insert 1 1 [ "b" ]
        >>= insert 1 1 [ "a" ]

    let multipleValues =
        emptyDocument
        >>= insert 0 0 [ "a" ; "b" ; "c" ; "d" ]

    match chained with
        | Ok chainedDoc ->
            match multipleValues with
                | Ok multipleDoc ->
                    Assert.Equal(chainedDoc, multipleDoc)
                | Error s -> fail s
        | Error s -> fail s
       
[<Fact>]
let ``Insert updates isModified flag`` () =
    emptyDocument
    >>= insert 0 0 [ "a" ]
    |> function
        | Ok doc -> Assert.True(doc.IsModified)
        | Error s -> Assert.True(false, s)

[<Fact>]
let ``Insert updates Length`` () =
    emptyDocument
    >>= insert 0 0 [ "a" ; "b" ; "c" ]
    |> function
        | Ok doc -> Assert.Equal(3, doc.Length)
        | Error s -> fail s
                 
////////////////////////////////////////////////////////////////////////////////
// append tests
////////////////////////////////////////////////////////////////////////////////

[<Fact>]
let ``Append on an empty document replaces contents`` () =
    emptyDocument
    >>= append 0 0 [ "a" ]
    >>= toTestString
    |> function
        | Ok s -> Assert.Equal("a", s)
        | Error s -> fail s
    
[<Fact>]
let ``Append in succession creates multiple consecutive lines`` () =
    emptyDocument
    >>= append 0 0 [ "a" ]
    >>= append 1 1 [ "b" ]
    >>= append 2 2 [ "c" ]
    >>= toTestString
    |> function
        | Ok s -> Assert.Equal("a$b$c", s)
        | Error s -> fail s
        
[<Fact>]
let ``Append sets IsModified flag`` () =
     emptyDocument
    >>= append 0 0 [ "a" ]
    |> function
        | Ok doc -> Assert.True(doc.IsModified)
        | Error s -> fail s

[<Fact>]
    emptyDocument
    >>= append 0 0 [ "a" ]
    >>= append 1 1 [ "b" ]
    >>= append 1 1 [ "c" ]
    |> function
        | Ok doc -> Assert.Equal(3, doc.Length)
        | Error s -> fail s

////////////////////////////////////////////////////////////////////////////////
// remove tests
////////////////////////////////////////////////////////////////////////////////

[<Fact>]
let ``Remove on one line returns Empty document`` () =
    emptyDocument
    >>= append 0 0 [ "a" ]
    >>= remove 1 1
    >>= toTestString
    |> function
        | Ok s -> Assert.Equal("", s)
        | Error s -> fail s

[<Fact>]
let ``Remove removes only one line if requested to do so`` () =
    emptyDocument
    >>= append 0 0 [ "a" ; "b" ; "c" ]
    >>= remove 2 2
    >>= toTestString
    |> function
        | Ok s -> Assert.Equal("a$c", s)
        | Error s -> fail s

[<Fact>]
let ``Removing a region is equivalent to chained removes`` () =
    let chained =
        emptyDocument
        >>= append 0 0 [ "a" ; "b" ; "c" ]
        >>= remove 1 1
        >>= remove 1 1

    let range =
        emptyDocument
        >>= append 0 0 [ "a" ; "b" ; "c" ]
        >>= remove 1 2

    match chained with
        | Ok chainedDoc ->
            match range with
                | Ok rangeDoc ->
                    Assert.Equal(chainedDoc, rangeDoc)
                | Error s -> fail s
        | Error s -> fail s

////////////////////////////////////////////////////////////////////////////////
// list tests
////////////////////////////////////////////////////////////////////////////////

let abcDocument =
    emptyDocument
    >>= append 0 0 [ "a" ; "b" ; "c" ]

[<Fact>]
let ``list returns untouched document`` () =
    abcDocument
    >>= list 1 1
    |> function
        | Ok (_, doc) ->
            Assert.Equal(abcDocument, Ok doc)
        | Error s -> fail s
            

[<Fact>]
let ``list displays single line if start = end`` () =
    emptyDocument
    >>= append 0 0 [ "a" ; "b" ; "c" ]
    >>= list 1 1
    |> function
        | Ok (strlist, doc) ->
            Assert.Equal("a", (String.concat "" strlist))
        | Error s -> fail s

[<Fact>]
let ``list displays proper range when requirested`` () =
    abcDocument
    >>= list 1 2
    |> function
        | Ok (strlist, _) ->
            Assert.Equal("a$b", (String.concat "$" strlist))
        | Error s -> fail s

////////////////////////////////////////////////////////////////////////////////
// change tests
////////////////////////////////////////////////////////////////////////////////
                                                         
[<Fact>]
let ``change changes value from one to another`` () =
    abcDocument
    >>= change 1 1 [ "g" ]
    >>= toTestString
    |> function
        | Ok s -> Assert.Equal("g$b$c", s)
        | Error s -> fail s

[<Fact>]
let ``change in middle of document works`` () =
    abcDocument
    >>= change 2 2 [ "g" ]
    >>= toTestString
    |> function
        | Ok s -> Assert.Equal("a$g$c", s)
        | Error s -> fail s

[<Fact>]
let ``change at the end of the document works`` () =
    abcDocument
    >>= change 3 3 [ "g" ]
    >>= toTestString
    |> function
        | Ok s -> Assert.Equal("a$b$g", s)
        | Error s -> fail s
