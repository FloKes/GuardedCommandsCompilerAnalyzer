let path = __SOURCE_DIRECTORY__ + "\programGraph.dot"
let init f=
    System.IO.File.WriteAllText( path, "digraph ProgramGraph{")

let writeLine s=
    System.IO.File.AppendAllText( path, "\n" + s)

let nodeText n =
    "\"q" + (string n) + "\""

let labelText s=
    "[label=\" " + s + " \"]"


let finish f=
    System.IO.File.AppendAllText( path, "\n}")

// let writeProgramGraph edges =
//     List.iter (fun x -> 
//                 match x with
//                     | Edge(s, act, e) -> writeLine (nodeText(s) + " -> " + nodeText(e) + labelText(act))  ) edges


// let printProgramGraph e =
//     init 0
//     writeProgramGraph(e)
//     finish 0

