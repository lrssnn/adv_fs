module adv_fs.Day15

open Puz.Input
open Puz.Convenience

type Tile =
    | Wall
    | Box
    | Space

    static member FromChar c =
        match c with
        | '#' -> Tile.Wall
        | 'O' -> Tile.Box
        | '.' -> Tile.Space
        | '@' -> Tile.Space
        | _ -> failwith $"Invalid tile char {c}"

    member t.ToString =
        match t with
        | Tile.Wall -> "#"
        | Tile.Box -> "O"
        | Tile.Space -> "."

type Coord =
    { x: int
      y: int }

    static member (+)(a, b) = { x = a.x + b.x; y = a.y + b.y }

    static member FromChar c =
        match c with
        | '>' -> { x = 1; y = 0 }
        | '<' -> { x = -1; y = 0 }
        | '^' -> { x = 0; y = -1 }
        | 'v' -> { x = 0; y = 1 }
        | _ -> failwith $"Invalid movement char {c}"

type Day15() =
    let input = readText "input/input15.txt"

    let testInput =
        "########\n\
        #..O.O.#\n\
        ##@.O..#\n\
        #...O..#\n\
        #.#.O..#\n\
        #...O..#\n\
        #......#\n\
        ########\n\
\n\
        <^^>>>vv<v>>v<<"

    let parseMap input =
        let tiles = input |> readAsCharA2D |> Array2D.map Tile.FromChar
        let lines = input |> readLines

        let y = lines |> Array.findIndex (String.exists (fun c -> c = '@'))

        let x = lines[y] |> Seq.findIndex (fun c -> c = '@')

        tiles, { x = x; y = y }

    let parseSeq input =
        input |> Seq.filter (fun c -> not (c = '\n')) |> Seq.map Coord.FromChar

    let toString (tiles: Tile array2d) robot =
        let strs =
            tiles
            |> Array2D.mapi (fun y x tile -> if x = robot.x && y = robot.y then "@" else tile.ToString)

        [ 0 .. (Array2D.length1 strs) - 1 ]
        |> List.map (fun y -> strs[y, 0..])
        |> List.map (String.concat "")
        |> String.concat "\n"

    let parse input =
        let mapS, sequenceS = splitOnceStr "\n\n" input
        let map, robot = parseMap mapS
        let sequence = parseSeq sequenceS

        map, sequence, robot

    let rec tryFindSpace (tiles: Tile array2d) from move =
        let nextLoc = from + move

        match tiles[nextLoc.y, nextLoc.x] with
        | Space -> Some nextLoc // Found
        | Wall -> None // Cannot move
        | Box -> tryFindSpace tiles nextLoc move // Indeterminate, need to try again

    let processMove (tiles: Tile array2d) (robot: Coord) (move: Coord) =
        let nextLoc = robot + move
        let nextTile = tiles[nextLoc.y, nextLoc.x]

        match nextTile with
        | Space -> tiles, nextLoc // Just move
        | Wall -> tiles, robot // Can't do anything
        | Box ->
            // Determine if we can move the box by finding a space to push the stack into
            match tryFindSpace tiles nextLoc move with
            | None -> tiles, robot // Can't move
            | Some(toMoveLoc) ->
                tiles[nextLoc.y, nextLoc.x] <- Tile.Space
                tiles[toMoveLoc.y, toMoveLoc.x] <- Tile.Box
                tiles, nextLoc

    let rec processMoves tiles robot moves =
        moves |> Seq.fold (fun (tiles, robot) -> processMove tiles robot) (tiles, robot)
    // if Seq.isEmpty moves then
    //     //printfn $"Final state:"
    //     //printfn $"{toString tiles robot}"
    //     tiles, robot
    // else
    //     //printfn $"Processing {Seq.head moves} with state"
    //     //printfn $"{toString tiles robot}"
    //     let nextTiles, nextRobot = processMove tiles robot (Seq.head moves)
    //     processMoves nextTiles nextRobot (Seq.tail moves)

    let scoreTiles tiles =
        tiles
        |> Array2D.mapi (fun y x tile ->
            match tile with
            | Tile.Box -> 100 * y + x
            | _ -> 0)
        |> Seq.cast<int>
        |> Seq.sum

    interface Day with
        member this.DayName = "15"
        member this.answer1 = "1441031"
        member this.answer2 = "?"

        member this.part1() =
            let map, moves, robot = parse input
            let nextMap, nextRobot = processMoves map robot moves
            scoreTiles nextMap |> string

        member this.part2() = "!"
