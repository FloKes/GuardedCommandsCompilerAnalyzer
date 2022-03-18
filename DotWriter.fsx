#load "ActionToText.fs"
open ActionToText

let path = __SOURCE_DIRECTORY__ + "\programGraph.dot"
let init f=
    System.IO.File.WriteAllText( path, "digraph ProgramGraph{")

let writeLine s=
    System.IO.File.AppendAllText( path, "\n" + s)

let nodeText n =
    "\"q" + (string n) + "\""

let labelText act=
    let s =  ActionToText.printAction act
    "[label=\" " + s + " \"]"



let finish f=
    System.IO.File.AppendAllText( path, "\n}")

let writeLines (edges: list<edgeTypes>) =
    List.iteri (fun i x -> 
                match x with
                    | Edge(s, act, e) -> writeLine (nodeText(s) + " -> " + nodeText(e) + labelText(act))  ) edges


let writeProgramGraph e =
    init 0
    writeLines(e)
    finish 0

