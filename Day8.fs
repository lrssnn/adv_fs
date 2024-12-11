module adv_fs.Day8

open Puz.Input

// TODO: merge this and day6s into a reusable 2d library
type Coord =
    { x: int
      y: int }

    static member (+)(a, b) = { x = a.x + b.x; y = a.y + b.y }
    static member (-)(a, b) = { x = a.x - b.x; y = a.y - b.y }

type Day8() =
    let input = readText "input/input08.txt"

    let testInput =
        "............\n\
        ........0...\n\
        .....0......\n\
        .......0....\n\
        ....0.......\n\
        ......A.....\n\
        ............\n\
        ............\n\
        ........A...\n\
        .........A..\n\
        ............\n\
        ............"

    let parse input : (Map<char, Coord list> * Coord) =
        let lines = readLines input

        let antennae =
            lines
            |> List.ofArray
            |> List.indexed
            |> List.collect (fun (y, line) ->
                line
                |> Seq.indexed
                |> Seq.choose (fun (x, char) -> if char = '.' then None else Some(char, { x = x; y = y }))
                |> List.ofSeq)
            |> List.groupBy fst
            |> List.map (fun (c, list) -> (c, List.map snd list))
            |> Map.ofList

        let bounds =
            { x = lines[0].Length
              y = lines.Length }

        (antennae, bounds)

    let getAntinodes a b =
        let offset = b - a
        [ a - offset; b + offset ]

    let allAntinodes antennae =
        antennae
        |> List.pairwise
        |> List.collect (fun (a, b) -> getAntinodes a b)
        |> Set.ofList

    let toString label antennae nodes bounds =
        [ 0 .. bounds.y ]
        |> List.map (fun y ->
            [ 0 .. bounds.x ]
            |> List.map (fun x ->
                if List.contains { x = x; y = y } antennae then label
                else if Set.contains { x = x; y = y } nodes then "#"
                else ".")
            |> String.concat "")
        |> String.concat "\n"

    interface Day with
        member this.DayName = "07"
        member this.answer1 = "?"
        member this.answer2 = "?"

        member this.part1() =
            let (antennae, bounds) = parse testInput
            // Do something!
            antennae
            |> Map.map (fun c list ->
                let nodes = allAntinodes list
                //printfn $"\n{toString (string c) list nodes bounds}\n"
                nodes)
            |> Map.values
            |> Set.unionMany
            |> Set.count
            |> string

        member this.part2() = "?"
