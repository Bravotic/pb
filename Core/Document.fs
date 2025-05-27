module Document

// Represents a document buffer to which text can be edited.
type Document = {
    Data : Rope.Rope
    IsModified : bool
    Length : int
}

// Represents a line number in a Document. Note that lines are 1 indexed, therefore the
// first line in a file is 1.
type LineNum = int

let EmptyDocument = { Data = Rope.Empty
                      IsModified = false
                      Length = 0 }

// string -> Document -> string
// Creates a string from the given document. Lines are joined with the specified line
// ending. Note: Line endings are not added to the final line of the file.
let toString (lineEnding : string) (doc : Document) =
    doc.Data
    |> Rope.toList
    |> String.concat lineEnding

// Rope -> Document
// Helper function to update the Rope data and set the IsModified flag and Length.
let private updateDocument (r : Rope.Rope) =
    { Data = r ; IsModified = true ; Length = Rope.size r }

// string list -> LineNum -> LineNum -> Document -> Document
// Inserts all values given into the document at the specified start line. The ending line is ignored here.
let private insertUnsafe (values : string list) (startLine : LineNum) (endLine : LineNum) (doc : Document) =
    let clampedStartLine = max 0 (startLine - 1)
    
    Rope.insertAll clampedStartLine values doc.Data
    |> updateDocument

// string list -> LineNum -> LineNum -> Document -> Document
// Appends all values given after the start line specified. The end line is also ignored here.
let private appendUnsafe (values : string list) (startLine : LineNum) (endLine : LineNum) (doc : Document) =
    Rope.insertAll startLine values doc.Data
    |> updateDocument

// int -> int > Document -> Document
// Removes the range from startLine to endLine inclusive from the document and returns the
// updated Document.
let private removeUnsafe (startLine : LineNum) (endLine : LineNum) (doc : Document) =
    let clampedStartLine = max 0 (startLine - 1)
    let count = endLine - startLine
    
    Rope.removeAll clampedStartLine count doc.Data
    |> updateDocument

// int -> int -> Document -> (string list, Document)
// Gets the values from startLine to endLine inclusive and collects them into a string list.
// Returns a tuple of the resulting string liste along side the Document provided which is
// unchanged.
let private listUnsafe (startLine : LineNum) (endLine : LineNum) (doc : Document) =
    ((Rope.getAll (startLine - 1) (endLine - 1) doc.Data), doc)
    
////////////////////////////////////////////////////////////////////////////////
// The following code is backend agnostic, meaning if Rope was removed as the
// data source, the following lines would not need to be updated.

let private validateParameters (startLine : LineNum) (endLine : LineNum) (doc : Document) fn =
    if startLine > endLine then
        Error "Starting line cannot be larger than ending line"
    elif startLine <= 0 then
        Error "Starting line must be within the document"
    elif endLine > doc.Length then
        Error "Ending line must be within the document"
    else
        Ok (fn startLine endLine doc)

let private validateParametersInsApp startLine endLine doc fn =
    if startLine = 0 && doc.Length = 0 then
        Ok (fn startLine endLine doc)
    else
        validateParameters startLine endLine doc fn

// LineNum -> LineNum -> String list -> Document -> Result<Document>
// Calls insertUnsafe while validating parameters given.
let insert (startLine : LineNum) (_ : LineNum) (values : string list) (doc : Document) =
    insertUnsafe values
    |> validateParametersInsApp startLine startLine doc
   
// LineNum -> string -> Document -> Result<Document>
// Applies appendUnsafe while validating parameters given
let append (startLine : LineNum) (_ : LineNum) (values : string list) (doc : Document) =
    appendUnsafe values 
    |> validateParametersInsApp startLine startLine doc

// LineNum -> LineNum -> Document -> Result<Document>
// Applies removeUnsafe while v
let remove (startLine : LineNum) (endLine : LineNum) (doc : Document) =
    removeUnsafe
    |> validateParameters startLine endLine doc

// LineNum -> LineNum -> Document -> Result<(string list, Document)>
// Lists all lines from startLine to endLine inclusive. 
let list (startLine : LineNum) (endLine : LineNum) (doc : Document) =
    listUnsafe
    |> validateParameters startLine endLine doc

// LineNum -> string list -> Document -> Result<Document>
// Helper function to perform an insert at a position, however if that position
// is after the end of the document (for example at Length + 1), the appropriate
// append command is called instead.
let private insertWrap position values doc =
    if position <= doc.Length then
        insert position position values doc
    else
        append doc.Length doc.Length values doc

let change (startLine : LineNum) (endLine : LineNum) (values : string list) (doc : Document) =
    remove startLine endLine doc
    |> Result.bind (insertWrap startLine values)
  
