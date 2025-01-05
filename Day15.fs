module adv_fs.Day15

open Puz.Input
open Puz.Convenience

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

    member t.isVertical =
        match t with
        | { x = 1; y = 0 } -> false
        | { x = -1; y = 0 } -> false
        | { x = 0; y = -1 } -> true
        | { x = 0; y = 1 } -> true
        | _ -> failwith $"Invalid move {t}"

type Tile =
    | Wall
    | Box
    | BoxL
    | BoxR
    | Space

    static member FromChar c =
        match c with
        | '#' -> Tile.Wall
        | 'O' -> Tile.Box
        | '.' -> Tile.Space
        | '@' -> Tile.Space
        | _ -> failwith $"Invalid tile char {c}"

    static member expandFromChar c =
        match c with
        | '#' -> [ Wall; Wall ]
        | 'O' -> [ BoxL; BoxR ]
        | '.' -> [ Space; Space ]
        | '@' -> [ Space; Space ]
        | _ -> failwith $"Invalid tile char {c}"

    member t.neighbour =
        match t with
        | Tile.BoxL -> { x = +1; y = 0 }
        | Tile.BoxR -> { x = -1; y = 0 }
        | _ -> failwith $"Cannot ask for neighbour of {t}"

type Day15() =
    let input = readText "input/input15.txt"

    let findRobot input =
        let lines = input |> readLines
        let y = lines |> Array.findIndex (String.exists (fun c -> c = '@'))
        let x = lines[y] |> Seq.findIndex (fun c -> c = '@')
        { x = x; y = y }

    let parseMap input =
        let tiles = input |> readAsCharA2D |> Array2D.map Tile.FromChar
        tiles, (findRobot input)

    let expandMap input =
        let tiles =
            input
            |> readLines
            |> Array.map (fun line -> line |> Seq.collect Tile.expandFromChar)
            |> array2D

        let loc = findRobot input
        tiles, { loc with x = loc.x * 2 }

    let parseSeq input =
        input |> Seq.filter (fun c -> not (c = '\n')) |> Seq.map Coord.FromChar

    let parse input expand =
        let mapS, sequenceS = splitOnceStr "\n\n" input
        let map, robot = if expand then expandMap mapS else parseMap mapS
        let sequence = parseSeq sequenceS

        map, sequence, robot

    let rec canMove (tiles: Tile array2d) from (move: Coord) =
        let nextLoc = from + move
        let nextTile = tiles[nextLoc.y, nextLoc.x]

        match nextTile with
        | Space -> true // Found
        | Wall -> false // Cannot move
        | BoxL
        | BoxR when move.isVertical ->
            (canMove tiles nextLoc move)
            && (canMove tiles (nextLoc + nextTile.neighbour) move)
        | _ -> canMove tiles nextLoc move // Indeterminate, need to try again

    let rec applyMove (tiles: Tile array2d) from (move: Coord) =
        let nextLoc = from + move
        let nextTile = tiles[nextLoc.y, nextLoc.x]

        match nextTile with
        | Wall -> failwith "applyMove should not ever reach a wall"
        | Space -> () // Already have space to move into
        | BoxL
        | BoxR when move.isVertical -> // Move the box blocking us, then move our box
            // Move the counterpart box
            applyMove tiles (nextLoc + nextTile.neighbour) move
            applyMove tiles nextLoc move
        | _ -> applyMove tiles nextLoc move

        tiles[nextLoc.y, nextLoc.x] <- tiles[from.y, from.x]
        tiles[from.y, from.x] <- Tile.Space

    let processMove (tiles: Tile array2d) (robot: Coord) (move: Coord) =
        let nextLoc = robot + move
        let nextTile = tiles[nextLoc.y, nextLoc.x]

        let moveSolved =
            match nextTile with
            | Wall -> false
            | Space -> true
            | BoxL
            | BoxR when move.isVertical ->
                (canMove tiles nextLoc move)
                && (canMove tiles (nextLoc + nextTile.neighbour) move)
            | _ -> canMove tiles nextLoc move

        if not moveSolved then
            tiles, robot
        else
            match nextTile with
            | Wall -> failwith "Shouldn't try to move wall"
            | Space -> ()
            | BoxL
            | BoxR when move.isVertical ->
                // Similar to box, but need to check if we can move the left box as well,
                // And then commit with a recursive push to preserve order
                applyMove tiles nextLoc move
                applyMove tiles (nextLoc + nextTile.neighbour) move
            | _ -> applyMove tiles nextLoc move

            tiles, nextLoc

    let rec processMoves tiles robot moves =
        moves |> Seq.fold (fun (tiles, robot) -> processMove tiles robot) (tiles, robot)

    let scoreTiles tiles =
        tiles
        |> Array2D.mapi (fun y x tile ->
            match tile with
            | Tile.BoxL
            | Tile.Box -> 100 * y + x
            | _ -> 0)
        |> Seq.cast<int>
        |> Seq.sum

    let solve input expand =
        let map, moves, robot = parse input expand
        let nextMap, _nextRobot = processMoves map robot moves
        scoreTiles nextMap

    interface Day with
        member this.DayName = "15"
        member this.answer1 = "1441031"
        member this.answer2 = "1425169"

        member this.part1() = solve input false |> string

        member this.part2() = solve input true |> string
