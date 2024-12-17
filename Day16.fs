module adv_fs.Day16

open Puz.Input

type Direction =
    | Up
    | Down
    | Left
    | Right

    member d.ToString =
        match d with
        | Up -> "^"
        | Down -> "v"
        | Left -> "<"
        | Right -> ">"

type Tile =
    | Wall
    | Space
    | Start
    | Goal

    static member FromChar c =
        match c with
        | '#' -> Tile.Wall
        | '.' -> Tile.Space
        | 'S' -> Tile.Start
        | 'E' -> Tile.Goal
        | _ -> failwith $"Invalid tile char {c}"

    member t.ToString =
        match t with
        | Tile.Wall -> "#"
        | Tile.Space -> "."
        | Tile.Start -> "S"
        | Tile.Goal -> "E"

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

type Move = { Pos: Coord; StepDir: Direction }

type Day16() =
    let input = readText "input/input16.txt"

    let testInput =
        "###############\n\
        #.......#....E#\n\
        #.#.###.#.###.#\n\
        #.....#.#...#.#\n\
        #.###.#####.#.#\n\
        #.#.#.......#.#\n\
        #.#.#####.###.#\n\
        #...........#.#\n\
        ###.#.#####.#.#\n\
        #...#.....#.#.#\n\
        #.#.#.###.#.#.#\n\
        #.....#...#.#.#\n\
        #.###.#.#.#.#.#\n\
        #S..#.....#...#\n\
        ###############"

    let locateTile tiles target =
        let y = tiles |> List.findIndex (List.exists (fun tile -> tile = target))
        let x = tiles[y] |> List.findIndex (fun tile -> tile = target)
        { x = x; y = y }

    let parseMap input =
        let tiles = input |> readAsMappedListList Tile.FromChar
        let start = locateTile tiles Tile.Start
        let goal = locateTile tiles Tile.Goal
        tiles, start, goal

    let toString (tiles: Tile list list) curr =
        tiles
        |> List.mapi (fun y row ->
            row
            |> List.mapi (fun x tile -> if x = curr.x && y = curr.y then "@" else tile.ToString)
            |> String.concat "")
        |> String.concat "\n"

    // let getPossibleMoves tile lastMove =


    // let rec solve tiles (path: Move list) (goal: Coord): int =
    //     if path.Head.Pos = goal then
    //         score path
    //     else
    //         let possibleMoves = getPossibleMoves tiles path.Head
    //         possibleMoves
    //         |> Seq.map (fun move -> tiles (move::path) goal)
    //         |> Seq.min

    interface Day with
        member this.DayName = "16"
        member this.answer1 = "?"
        member this.answer2 = "?"

        member this.part1() =
            let map, start, goal = parseMap testInput
            "!"

        member this.part2() = "!"
