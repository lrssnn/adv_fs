module adv_fs.Day6

open Puz.Convenience
open Puz.Input
open FSharpx.String

type Direction =
    | Up
    | Down
    | Left
    | Right

type Coord =
    { i: int
      j: int }

    static member (+)(a, b) = { i = a.i + b.i; j = a.j + b.j }

type SimResult =
    | LeftMap of Coord Set
    | Looped

type Guard = { pos: Coord; dir: Direction }

type Day6() =
    let input = readText "input/input06.txt"

    let tryIntoDirection (char: char) : Direction option =
        match char with
        | '^' -> Some(Direction.Up)
        | '>' -> Some(Direction.Right)
        | '<' -> Some(Direction.Left)
        | 'v' -> Some(Direction.Down)
        | _ -> None

    let tryFindGuardInLine (line: string) =
        line
        |> Seq.indexed
        |> Seq.choose (fun (j, char) ->
            match tryIntoDirection char with
            | Some(d) -> Some(j, d)
            | None -> None)
        |> Seq.tryExactlyOne

    let parseChar c = c = '#'

    let parse (input: string) : (bool array2d * Guard) =
        let lines = input |> splitChar [| '\n' |]
        let map = lines |> Array.map (Seq.map parseChar) |> array2D

        let guard =
            lines
            |> Array.indexed
            |> Array.choose (fun (i, line) ->
                match tryFindGuardInLine line with
                | Some(j, d) -> Some(i, j, d)
                | None -> None)
            |> Array.exactlyOne
            |> (fun (i, j, d) -> { pos = { i = i; j = j }; dir = d })

        (map, guard)

    let nextDirection direction =
        match direction with
        | Direction.Down -> Direction.Left
        | Direction.Up -> Direction.Right
        | Direction.Left -> Direction.Up
        | Direction.Right -> Direction.Down

    let offsetOf direction =
        match direction with
        | Direction.Down -> { i = 1; j = 0 }
        | Direction.Up -> { i = -1; j = 0 }
        | Direction.Left -> { i = 0; j = -1 }
        | Direction.Right -> { i = 0; j = 1 }

    let nextSpace (guard: Guard) =
        let offset = offsetOf guard.dir
        guard.pos + offset

    let rec nextGuard (guard: Guard) (map: bool array2d) overrideCoord : Guard =
        // Try step forward
        let nextPos = nextSpace guard
        // If that space is an obstacle, turn right and try again
        if
            (inbounds map nextPos.i nextPos.j) && map[nextPos.i, nextPos.j]
            || (nextPos = overrideCoord)
        then
            nextGuard
                { guard with
                    dir = nextDirection guard.dir }
                map
                overrideCoord
        else
            { guard with pos = nextPos }

    let rec getVisited overrideCoord map guard acc =
        if not (inbounds map guard.pos.i guard.pos.j) then
            // Acc contains duplicate locations where we have visited it going different directions,
            // Remove them by dropping the direction from the set before returning
            LeftMap(Set.map (fun g -> g.pos) acc)
        else if Set.contains guard acc then
            Looped
        else
            let nextGuard = nextGuard guard map overrideCoord
            getVisited overrideCoord map nextGuard (Set.add guard acc)


    interface Day with
        member this.DayName = "06"
        member this.answer1 = "5101"
        member this.answer2 = "1951"

        member this.part1() =
            let (map, guard) = parse input
            let simResult = getVisited { i = -1; j = -1 } map guard Set.empty

            let l =
                match simResult with
                | LeftMap set -> Set.count set
                | Looped -> failwith "Looped in part one"

            string (l)

        member this.part2() =
            let (map, guard) = parse input

            let candidates =
                match getVisited { i = -1; j = -1 } map guard Set.empty with
                | LeftMap c -> c
                | Looped -> failwith "looped on initial input"

            let loops =
                candidates
                |> Set.filter (fun c ->
                    // if x = i && y = j then
                    //     false
                    // else
                    match getVisited c map guard Set.empty with
                    | LeftMap _ -> false
                    | Looped -> true)
                |> Set.count

            string (loops)
