module DocumentTest 

open System
open Xunit
open Document

let emptyDocument = Ok { Data = Rope.Empty 
                         IsModified = false
                         Selection = { Start = 0 ; End = 0 }
                         Length = 0 }

let abDocument = Ok { Data = Rope.fromList [ "a" ; "b" ]
                      IsModified = true
                      Selection = { Start = 1 ; End = 1 }
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
// Selection tests
////////////////////////////////////////////////////////////////////////////////

[<Fact>]
let ``Line number 0 is valid on empty document`` =
    emptyDocument
    >>= select 0 0
    |> function
        | Ok s -> Assert.True(true)
        | Error s -> fail s

[<Fact>]
let ``Line number 0 is invalid on document with content`` =
    abDocument
    >>= select 0 0
    |> function
        | Ok s -> fail "Expected an error"
        | Error s -> Assert.True(true)

[<Fact>]
let ``Line number other than zero is invalid on empty document`` =
    emptyDocument
    >>= select 1 1
    |> function
        | Ok s -> fail "Expected an error"
        | Error s -> Assert.True(true)

[<Fact>]
let ``Selecting a line number outside the document causes an error`` =
    abDocument
    >>= select 1 3
    |> function
        | Ok s -> fail "Expected an error"
        | Error s -> Assert.True(true)

[<Fact>]
let ``Selection is updated on document with content when selection is made`` =
    abDocument
    >>= select 1 2
    |> function
        | Ok ({ Selection = { Start = start ; End = ending } }) ->
            Assert.Equal(1, start)
            Assert.Equal(2, ending)
        | Error s -> fail s

[<Fact>]
let ``Selection on empty document stays at 0 0`` =
    emptyDocument
    >>= select 0 0
    |> function
        | Ok ({ Selection = { Start = start ; End = ending } }) ->
            Assert.Equal(0, start)
            Assert.Equal(0, ending)
        | Error s -> fail s

        
////////////////////////////////////////////////////////////////////////////////
// insert tests
////////////////////////////////////////////////////////////////////////////////

[<Fact>]
let ``Insert on a blank document gives us just one line`` () =
    emptyDocument
    >>= insert [ "a" ]
    >>= toTestString
    |> function
        | Ok s -> Assert.Equal("a", s)
        | Error s -> fail s

[<Fact>]
let ``Insert chained gives us multiple lines`` () =
    emptyDocument
    >>= insert [ "c" ]
    >>= insert [ "b" ]
    >>= insert [ "a" ]
    >>= toTestString
    |> function
        | Ok s -> Assert.Equal("a$b$c", s)
        | Error s -> Assert.True(false, s)

[<Fact>]
let ``Insert puts value before selection`` () =
    abDocument
    >>= move 1
    >>= insert [ "Line" ]
    >>= toTestString
    |> function
        | Ok s -> Assert.Equal("Line$a$b", s)
        | Error s -> fail s

[<Fact>]
let ``Selection is set as last line of insert`` () =
    abDocument
    >>= insert [ "Line 1" ; "Line 2" ]
    |> function
        | Ok {Selection = {Start = s ; End = e }} ->
            Assert.Equal(2, s)
            Assert.Equal(2, e)
        | Error s -> fail s
        
[<Fact>]
let ``Chained calls are equivalent to one call with multiple values`` () =
    let chained =
        emptyDocument
        >>= insert [ "d" ]
        >>= insert [ "c" ]
        >>= insert [ "b" ]
        >>= insert [ "a" ]
        >>= toTestString

    let multipleValues =
        emptyDocument
        >>= insert [ "a" ; "b" ; "c" ; "d" ]
        >>= toTestString

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
    >>= insert [ "a" ]
    |> function
        | Ok doc -> Assert.True(doc.IsModified)
        | Error s -> Assert.True(false, s)

[<Fact>]
let ``Insert updates Length`` () =
    emptyDocument
    >>= insert [ "a" ; "b" ; "c" ]
    |> function
        | Ok doc -> Assert.Equal(3, doc.Length)
        | Error s -> fail s


////////////////////////////////////////////////////////////////////////////////
// append tests
////////////////////////////////////////////////////////////////////////////////

[<Fact>]
let ``Append on an empty document replaces contents`` () =
    emptyDocument
    >>= append [ "a" ]
    >>= toTestString
    |> function
        | Ok s -> Assert.Equal("a", s)
        | Error s -> fail s
    
[<Fact>]
let ``Append in succession creates multiple consecutive lines`` () =
    emptyDocument
    >>= append [ "a" ]
    >>= append [ "b" ]
    >>= append [ "c" ]
    >>= toTestString
    |> function
        | Ok s -> Assert.Equal("a$b$c", s)
        | Error s -> fail s
        
[<Fact>]
let ``Append sets IsModified flag`` () =
     emptyDocument
    >>= append [ "a" ]
    |> function
        | Ok doc -> Assert.True(doc.IsModified)
        | Error s -> fail s

[<Fact>]
    emptyDocument
    >>= append [ "a" ]
    >>= append [ "b" ]
    >>= append [ "c" ]
    |> function
        | Ok doc -> Assert.Equal(3, doc.Length)
        | Error s -> fail s

////////////////////////////////////////////////////////////////////////////////
// remove tests
////////////////////////////////////////////////////////////////////////////////

[<Fact>]
let ``Remove on one line returns Empty document`` () =
    emptyDocument
    >>= append [ "a" ]
    >>= remove
    >>= toTestString
    |> function
        | Ok s -> Assert.Equal("", s)
        | Error s -> fail s

[<Fact>]
let ``Remove removes only one line if requested to do so`` () =
    emptyDocument
    >>= append [ "a" ; "b" ; "c" ]
    >>= move 2
    >>= remove
    >>= toTestString
    |> function
        | Ok s -> Assert.Equal("a$c", s)
        | Error s -> fail s

[<Fact>]
let ``Removing a region is equivalent to chained removes`` () =
    let chained =
        emptyDocument
        >>= append [ "a" ; "b" ; "c" ]
        >>= remove
        >>= remove

    let range =
        emptyDocument
        >>= append [ "a" ; "b" ; "c" ]
        >>= select 2 3
        >>= remove

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
    >>= append [ "a" ; "b" ; "c" ]

let ``successive calls to list moves selection forward by 1 each time`` () =
    abcDocument
    >>= move 1
    >>= list
    |> function
        | Ok (lst, doc) ->
            Assert.Equal({ Start = 2 ; End = 2 }, doc.Selection)
        | Error s -> fail s

[<Fact>]
let ``list displays single line if start = end`` () =
    emptyDocument
    >>= append [ "a" ; "b" ; "c" ]
    >>= move 1
    >>= list
    |> function
        | Ok (strlist, doc) ->
            Assert.Equal("a", (String.concat "" strlist))
        | Error s -> fail s

[<Fact>]
let ``list displays proper range when requirested`` () =
    abcDocument
    >>= select 1 2
    >>= list
    |> function
        | Ok (strlist, _) ->
            Assert.Equal("a$b", (String.concat "$" strlist))
        | Error s -> fail s
(*
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
*)
