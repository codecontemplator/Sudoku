// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

[<StructuredFormatDisplay("C v:{v} c:{c} r:{r}")>]
type Cell = { v: int Set; c : int; r : int; b : int } with
    static member create i v =           
        let (c,r) = (i%9,i/9)
        let b = r/3*3 + c/3
        match v with
        | 0 -> { v=Set.ofSeq [1..9]; c=c; r=r; b=b }
        | v -> { v=Set.singleton v; c=c; r=r; b=b } 
                             

module Seq =
    let all pred = Seq.fold (fun st x -> st && pred x) true

let createBoard str = 
    str |>
    Seq.map (fun ch -> int(ch) - int('0')) |> 
    Seq.toArray |>
    Array.mapi Cell.create

let problem = createBoard "000079065000003002005060093340050106000000000608020059950010600700600000820390000"

let emptyBoard = createBoard (String.replicate 81 "0")

let definites cell board =
    let adj c = 
        if c.r = cell.r && c.c = cell.c then 
            false
        else
            c.c = cell.c || c.r = cell.r || c.b = cell.b
    board |> Seq.filter (fun c -> Set.count c.v = 1) |> Seq.filter adj |> Seq.map (fun c -> c.v) |> Seq.concat |> Set.ofSeq

let rec propagateConstraints (board : Cell array) =    
    let board' = [| for cell in board do yield { cell with v = cell.v - definites cell board  } |]
    if board = board' then
        board'
    else
        propagateConstraints board'            

module Array =
    let rng = new System.Random()
    let shuffle (arr : 'a array) =
        let array = Array.copy arr
        let n = array.Length
        for x in 1..n do
            let i = n-x
            let j = rng.Next(i+1)
            let tmp = array.[i]
            array.[i] <- array.[j]
            array.[j] <- tmp
        array

type SolutionStrategy = First | All

let rec solveInternal strategy  (board : Cell array) : Cell array option =
    let board' = propagateConstraints board
    if Seq.exists (fun c -> Set.count c.v = 0) board' then
        None
    else
        match Seq.tryFindIndex (fun c -> Set.count c.v > 1) board' with
        | Some index -> 
            let cell = board'.[index]
            let alternatives = seq {
                for v in Array.shuffle (cell.v |> Set.toArray) do
                    let board'' = Array.copy board
                    board''.[index] <- { cell with v = Set.singleton v }
                    yield board''
            } 
            match strategy with
            | First ->
                match Seq.tryFind Option.isSome (Seq.map (solveInternal strategy) alternatives) with
                | Some solution -> solution
                | None -> None
            | All ->
                match Seq.filter Option.isSome (Seq.map (solveInternal strategy) alternatives) |> List.ofSeq with
                | [] -> None
                | [solution] -> solution
                | _ -> None
        | None -> Some board'

let solve board = solveInternal First board

let printBoard (board : Cell array) =
    let board' = board |> Array.sortBy (fun c -> c.r*9+c.c) 
    for j in 0..8 do
        for i in 0..8 do
            let cv = board'.[i+j*9].v
            if Set.count cv > 1 then
                printf "."
            else                
                printf "%A" (cv |> Set.toList |> List.head)
        printfn ""

let isValidSolution (boardOption : Cell array option) =
    match boardOption with 
    | Some board ->
        let allDefinite solution = Seq.exists (fun c -> Set.count c.v <> 1) solution |> not

        let groupValid gf = 
            let hasAllValues cells = cells |> Seq.map (fun c -> c.v) |> Seq.concat |> Set.ofSeq |> Set.count
            fun (solution : Cell array) -> Seq.all (fun x -> x = 9) (solution |> Seq.groupBy gf |> Seq.map snd |> Seq.map hasAllValues)

        let columnsValid = groupValid (fun c -> c.c)
        let rowsValid = groupValid (fun c -> c.r)
        let blocksValid = groupValid (fun c -> c.b)
    
        allDefinite board &&
        columnsValid board &&
        rowsValid board &&
        blocksValid board
    | None -> false

//let solution = solve problem
//isValidSolution solution |> printfn "%A"
    
let fullBoard() = solve emptyBoard |> Option.get

let generatePuzzle () =
    let candidates = [| 0..9*9-1 |] |> Array.shuffle |> List.ofArray
    let board = fullBoard()
    let rec reduce cs numGiven =
        if numGiven < 35 then
            board
        else 
            match cs with
            | [] -> board
            | i::cs -> 
                let backup = board.[i]
                board.[i] <- { board.[i] with v = Set.ofList [1..9] }                            
                if not(solveInternal All board |> isValidSolution) then
                    board.[i] <- backup
                    reduce cs numGiven
                else
                    reduce cs (numGiven-1)

    reduce candidates 81

[<EntryPoint>]
let main argv =     
    let b = generatePuzzle ()
    printfn "Puzzle:"
    printBoard b   
    let s = solve b
    printfn "Solution:"
    printBoard (Option.get s)
    0 // return an integer exit code
