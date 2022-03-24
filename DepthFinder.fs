module DepthFinder

let rec getDepthBool e d =
    d + 1

let rec getNumNodes e d =
    match e with
    | Assign(x,y) -> d + 1
    | Skip  -> d + 1
    | IfFi(x) -> getNumGuardedCommands x d
    | DoOd(x) -> getNumGuardedCommands x d
    | CommandSeq(x,y) -> getNumNodes x d + getNumNodes y d

and getNumGuardedCommands e d =
    match e with
    | GuardedCommand(x, y) -> getDepthBool x d + getNumNodes y d
    | GuardedCommandSeq(x,y) -> getNumGuardedCommands x d + getNumGuardedCommands y d

and getDepthCommand e d =
  match e with
    | Assign(x,y) -> d + 1
    | Skip  -> d + 1
    | IfFi(x) -> getDepthGuardedCommand x d
    | DoOd(x) -> getDepthGuardedCommand x d
    | CommandSeq(x,y) -> getNumNodes x d + getDepthCommand y d

and getDepthGuardedCommand e d=
    match e with
        | GuardedCommand(x, y) -> getDepthBool x d + getDepthCommand y d
        | GuardedCommandSeq(x,y) -> getDepthGuardedCommand x d



let getDepth command =
    let r = (getDepthCommand command 0) - 1
    printfn "Remaining nodes on command: %d" r
    r