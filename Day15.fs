module adv_fs.Day15

open Puz.Input
open Puz.Convenience

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

    member t.ToString =
        match t with
        | Tile.Wall -> "#"
        | Tile.Box -> "O"
        | Tile.Space -> "."
        | Tile.BoxL -> "["
        | Tile.BoxR -> "]"

    static member expandFromChar c =
        match c with
        | '#' -> [ Wall; Wall ]
        | 'O' -> [ BoxL; BoxR ]
        | '.' -> [ Space; Space ]
        | '@' -> [ Space; Space ]
        | _ -> failwith $"Invalid tile char {c}"

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

    member t.left = { t with x = t.x - 1 }
    member t.right = { t with x = t.x + 1 }

    member t.asMove =
        match t with
        | { x = 1; y = 0 } -> ">"
        | { x = -1; y = 0 } -> "<"
        | { x = 0; y = -1 } -> "^"
        | { x = 0; y = 1 } -> "v"
        | _ -> failwith $"Invalid move {t}"

    member t.isVertical =
        match t with
        | { x = 1; y = 0 } -> false
        | { x = -1; y = 0 } -> false
        | { x = 0; y = -1 } -> true
        | { x = 0; y = 1 } -> true
        | _ -> failwith $"Invalid move {t}"

    override this.ToString() = $"({this.x},{this.y})"

type Day15() =
    let input = readText "input/input15.txt"

    let testInput =
        "#######\n\
#...#.#\n\
#.....#\n\
#..OO@#\n\
#..O..#\n\
#.....#\n\
#######\n\
\n\
<vv<<^^<<^^"

    let testInput1 =
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

    let testInput2 =
        "##########\n\
#..O..O.O#\n\
#......O.#\n\
#.OO..O.O#\n\
#..O@..O.#\n\
#O#..O...#\n\
#O..O..O.#\n\
#.OO.O.OO#\n\
#....O...#\n\
##########\n\
\n\
<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^\n\
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v\n\
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<\n\
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^\n\
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><\n\
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^\n\
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^\n\
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>\n\
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>\n\
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"

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

    let toString (tiles: Tile array2d) robot =
        let strs =
            tiles
            |> Array2D.mapi (fun y x tile -> if x = robot.x && y = robot.y then "@" else tile.ToString)

        [ 0 .. (Array2D.length1 strs) - 1 ]
        |> List.map (fun y -> strs[y, 0..])
        |> List.map (String.concat "")
        |> String.concat "\n"

    let parse input expand =
        let mapS, sequenceS = splitOnceStr "\n\n" input
        let map, robot = if expand then expandMap mapS else parseMap mapS
        let sequence = parseSeq sequenceS

        map, sequence, robot

    let rec tryFindSpace (tiles: Tile array2d) from (move: Coord) =
        //printfn $"tryFindSpace {from} {move.asMove}"
        let nextLoc = from + move

        match tiles[nextLoc.y, nextLoc.x] with
        | Space ->
            //printfn "  Space, good"
            true // Found
        | Wall ->
            //printfn "  Wall, bad"
            false // Cannot move
        | BoxL when move.isVertical ->
            //printfn $"BoxL, need to check {nextLoc} and {nextLoc.right}"
            (tryFindSpace tiles nextLoc move) && (tryFindSpace tiles nextLoc.right move)
        | BoxR when move.isVertical ->
            //printfn $"BoxR, need to check {nextLoc} and {nextLoc.left}"
            (tryFindSpace tiles nextLoc move) && (tryFindSpace tiles nextLoc.left move)
        | _ ->
            //printfn $"Need to check {nextLoc}"
            tryFindSpace tiles nextLoc move // Indeterminate, need to try again

    let rec applyMove (tiles: Tile array2d) from (move: Coord) =
        //printfn $"applyMove {from} {move.asMove}"
        let nextLoc = from + move

        match tiles[nextLoc.y, nextLoc.x] with
        | Wall -> () // Cannot move
        | Space -> // Can move
            tiles[nextLoc.y, nextLoc.x] <- tiles[from.y, from.x]
            tiles[from.y, from.x] <- Tile.Space
        | BoxL when move.isVertical ->
            applyMove tiles nextLoc move
            tiles[nextLoc.y, nextLoc.x] <- tiles[from.y, from.x]
            tiles[from.y, from.x] <- Tile.Space
            // Move the counterpart box
            applyMove tiles nextLoc.right move
        | BoxR when move.isVertical -> // Move the box blocking us, then move our box
            applyMove tiles nextLoc move
            tiles[nextLoc.y, nextLoc.x] <- tiles[from.y, from.x]
            tiles[from.y, from.x] <- Tile.Space
            // Move the counterpart box
            applyMove tiles nextLoc.left move
        | _ ->
            applyMove tiles nextLoc move
            tiles[nextLoc.y, nextLoc.x] <- tiles[from.y, from.x]
            tiles[from.y, from.x] <- Tile.Space

    let processMove (tiles: Tile array2d) (robot: Coord) (move: Coord) =
        //printfn $"processMove {robot} {move.asMove}"
        let nextLoc = robot + move
        let nextTile = tiles[nextLoc.y, nextLoc.x]

        match nextTile with
        | Space -> tiles, nextLoc // Just move
        | Wall -> tiles, robot // Can't do anything
        | BoxR when move.isVertical ->
            //printfn $"Its a box R, checking if we can move it?"
            // Similar to box, but need to check if we can move the left box as well,
            // And then commit with a recursive push to preserve order
            if (tryFindSpace tiles nextLoc move) && (tryFindSpace tiles nextLoc.left move) then
                //printfn $"Yes we can... moving {nextLoc} {move.asMove}"
                applyMove tiles nextLoc move
                //printfn $"moving {nextLoc.left} {move.asMove}"
                applyMove tiles nextLoc.left move
                tiles, nextLoc
            else
                tiles, robot
        | BoxL when move.isVertical ->
            if (tryFindSpace tiles nextLoc move) && (tryFindSpace tiles nextLoc.right move) then
                applyMove tiles nextLoc move
                applyMove tiles nextLoc.right move
                tiles, nextLoc
            else
                tiles, robot
        | _ ->
            // Determine if we can move the box by finding a space to push the stack into
            if tryFindSpace tiles nextLoc move then
                //printfn $"Can move, moving"
                applyMove tiles nextLoc move
                tiles, nextLoc
            else
                //printfn $"Can't move"
                tiles, robot

    let rec processMoves tiles robot moves =
        moves
        |> Seq.fold
            (fun (tiles, robot) ->
                fun move ->
                    let t, r = processMove tiles robot move
                    //printfn $"{toString t r}"
                    t, r)
            (tiles, robot)
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
            | Tile.BoxL
            | Tile.Box -> 100 * y + x
            | _ -> 0)
        |> Seq.cast<int>
        |> Seq.sum

    interface Day with
        member this.DayName = "15"
        member this.answer1 = "1441031"
        member this.answer2 = "1425169"

        member this.part1() =
            let map, moves, robot = parse input false
            let nextMap, nextRobot = processMoves map robot moves
            scoreTiles nextMap |> string

        member this.part2() =
            let map, moves, robot = parse input true
            let nextMap, nextRobot = processMoves map robot moves
            scoreTiles nextMap |> string
