module Document

// Represents a line number in a Document. Note that lines are 1 indexed, therefore the
// first line in a file is 1.
type LineNum = int

type Selection =
    { Start : LineNum
      End : LineNum }

// Represents a document buffer to which text can be edited.
type Document = {
    Data : Rope.Rope
    IsModified : bool
    Selection : Selection
    Length : int
}

let EmptyDocument = { Data = Rope.Empty
                      IsModified = false
                      Selection = { Start = 0 ; End = 0 }
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
let private updateDocument ({ Selection = s } : Document) (r : Rope.Rope) =
    { Data = r ; IsModified = true ; Selection = s ; Length = Rope.size r }

let private setSelection (startLine : LineNum) (endLine : LineNum) (doc : Document) =
    let { Data = d ; IsModified = m ; Length = l } = doc
    
    { Data = d
      IsModified = m
      Length = l
      Selection = { Start = startLine ; End = endLine} }

let private updateSelection (start : LineNum) (lengthOfInsert : LineNum) (doc : Document) =
    let { Selection = s ; Length = l } = doc
    if l = 0 then
        setSelection lengthOfInsert lengthOfInsert doc
    else
        let newStart = start + lengthOfInsert
        setSelection newStart newStart doc

// LineNum -> LineNum -> Document -> Result<Document>
// Selects a region in a document which has no content. This is the only case where the line number
// 0 can be selected. In fact, it needs to be both the start and end line here...
let private selectEmptyDocument (startLine : LineNum) (endLine : LineNum) (doc : Document) =
    if startLine = 0 && endLine = 0 then
        Ok (setSelection startLine endLine doc)
    else
        Error "Document has no content, selection must be 0,0"
        
// LineNum -> LineNum -> Document -> Result<Document>
// Selects a region in a document which has content. This means the selection must be within the
// range of [1,doc.Length]
let private selectContentDocument (startLine : LineNum) (endLine : LineNum) (doc : Document) =
    if startLine < 1 then
        Error (sprintf "Start line must be within the range of [1,%d], got %d." doc.Length startLine)
    elif endLine > doc.Length then
         Error (sprintf "End line must be within the range of [1,%d], got %d." doc.Length startLine)
    else
        Ok (setSelection startLine endLine doc)

// LineNum -> LineNum -> Document -> Result<Document>
// Selects a region within the document for operations to occur at.
let select (startLine : LineNum) (endLine : LineNum) (doc : Document) =
    if doc.Length = 0 then
        selectEmptyDocument startLine endLine doc
    else
        selectContentDocument startLine endLine doc

// LineNum -> Document -> Result<Document>
// Moves the selection to a particular line. This is equivalent to selecting a single line.
let move (line : LineNum) (doc : Document) =
    select line line doc

// string list -> LineNum -> LineNum -> Document -> Document
// Inserts all values given into the document at the specified start line. The ending line is ignored here.
let insert (values : string list) (doc : Document) =
    let { Data = data ; Selection = s } = doc
    let clampedStartLine = max 0 (s.Start - 1)
            
    Rope.insertAll clampedStartLine values data
    |> updateDocument doc
    |> move (clampedStartLine + values.Length)

// string list -> LineNum -> LineNum -> Document -> Document
// Appends all values given after the start line specified. The end line is also ignored here.
let append (values : string list) (doc : Document) =
    let { Data = data ; Selection = s } = doc
    
    Rope.insertAll s.Start values data
    |> updateDocument doc
    |> move (s.Start + values.Length)
   
// int -> int > Document -> Document
// Removes the range from startLine to endLine inclusive from the document and returns the
// updated Document.
let remove (doc : Document) =
    let { Data = data ; Selection = s } = doc
    let clampedStartLine = max 0 (s.Start - 1)
    let count = s.End - s.Start
    let cursorAfterRemove =
        min s.Start ((doc.Length - count) - 1)
    
    Rope.removeAll clampedStartLine count data
    |> updateDocument doc
    |> move cursorAfterRemove

// int -> int -> Document -> (string list, Document)
// Gets the values from startLine to endLine inclusive and collects them into a string list.
// Returns a tuple of the resulting string liste along side the Document provided which is
// unchanged.
let list (doc : Document) =
    let { Data = data ; Selection = s } = doc
    let linesListed = max 1 (s.End - s.Start)
    let updatedDoc =
        doc
        |> updateSelection s.Start linesListed
        
    ((Rope.getAll (s.Start - 1) (s.End - 1) doc.Data), updatedDoc)
    |> Ok
    

let change (values : string list) (doc : Document) =
    remove doc
    |> Result.bind (insert values)
    
