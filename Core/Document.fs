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

// LineNum -> string -> Document
// Inserts the given value before the line number in the document.
let insert (line : LineNum) (value : string) (doc : Document) =
    Rope.insert doc.Data (line - 1) value |> updateDocument

// LineNum -> string -> Document
// Appends the given value after hte line number in the document.
let append (line : LineNum) (value : string) (doc : Document) =
    Rope.insert doc.Data line value |> updateDocument
