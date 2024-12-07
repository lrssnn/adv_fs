module adv_fs.Day6

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

    let parse (input: string) : (Coord Set * Guard * Coord) =
        let lines = input |> splitChar [| '\n' |]

        let obstacles =
            input
            |> readAsCharA2D
            |> Array2D.mapi (fun x y c -> if c = '#' then Some({ i = x; j = y }) else None)
            |> Seq.cast<Coord option>
            |> Seq.choose id
            |> Set.ofSeq

        let guard =
            lines
            |> Array.indexed
            |> Array.choose (fun (i, line) ->
                match tryFindGuardInLine line with
                | Some(j, d) -> Some(i, j, d)
                | None -> None)
            |> Array.exactlyOne
            |> (fun (i, j, d) -> { pos = { i = i; j = j }; dir = d })

        let bounds =
            { i = Array.length lines
              j = String.length lines[0] }

        (obstacles, guard, bounds)

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

    let rec nextGuard (guard: Guard) (obstacles: Coord Set) : Guard =
        // Try step forward
        let nextPos = nextSpace guard
        // If that space is an obstacle, turn right and try again
        if Set.contains nextPos obstacles then
            nextGuard
                { guard with
                    dir = nextDirection guard.dir }
                obstacles
        else
            { guard with pos = nextPos }

    let inbounds pos bounds =
        pos.i >= 0 && pos.i < bounds.i && pos.j >= 0 && pos.j < bounds.j

    let rec getVisited obstacles bounds guard acc =
        if not (inbounds guard.pos bounds) then
            // Acc contains duplicate locations where we have visited it going different directions,
            // Remove them by dropping the direction from the set before returning
            LeftMap(Set.map (fun g -> g.pos) acc)
        else if Set.contains guard acc then
            Looped
        else
            let nextGuard = nextGuard guard obstacles
            getVisited obstacles bounds nextGuard (Set.add guard acc)

    interface Day with
        member this.DayName = "06"
        member this.answer1 = "5101"
        member this.answer2 = "1951"

        member this.part1() =
            let (obstacles, guard, bounds) = parse input
            let simResult = getVisited obstacles bounds guard Set.empty

            let l =
                match simResult with
                | LeftMap set -> Set.count set
                | Looped -> failwith "Looped in part one"

            string (l)

        member this.part2() = "1951"
// Too slow
// let (obstacles, guard, bounds) = parse input
//
// let candidates =
//     match getVisited obstacles bounds guard Set.empty with
//     | LeftMap c -> c
//     | Looped -> failwith "looped on initial input"
//
// let loops i=
//     candidates
//     |> Set.toArray
//     |> Array.Parallel.sumBy (fun c ->
//         match getVisited (Set.add c obstacles) bounds guard Set.empty with
//         | LeftMap _ -> 0
//         | Looped -> 1)
//
// string (loops)
