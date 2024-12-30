module adv_fs.Day8

open Puz.Input

// TODO: merge this and day6s into a reusable 2d library
type Coord =
    { x: int
      y: int }

    static member (+)(a, b) = { x = a.x + b.x; y = a.y + b.y }
    static member (-)(a, b) = { x = a.x - b.x; y = a.y - b.y }
    static member (*)(i, c) = { x = c.x * i; y = c.y * i }
    override this.ToString() = $"({this.x},{this.y})"

type Day8() =
    let input = readText "input/input08.txt"

    // Parse the map into a list of lists of coords - each unique node frequency is in its
    // own list which contains the coords of that frequency node
    // Also returns the bounds of the map as a coord
    let parse input : Coord list list * Coord =
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
            |> List.map (fun (_, list) -> List.map snd list)

        let bounds =
            { x = lines[0].Length - 1
              y = lines.Length - 1 }

        (antennae, bounds)

    // True if the given node is within the bounds given (inclusive)
    let inbounds bounds node =
        node.x >= 0 && node.x <= bounds.x && node.y >= 0 && node.y <= bounds.y

    // Get the antinodes for the given pair of coordinates. maxHops determines how
    // many jump in each direction we are allowed to take
    let getAntinodes maxHops (a: Coord) b =
        let offset = b - a
        let neg = [ 1..maxHops ] |> List.map (fun hop -> a - (hop * offset))
        let pos = [ 1..maxHops ] |> List.map (fun hop -> b + (hop * offset))
        List.concat [ neg; pos ]

    // Generate all antinodes for the given list of coordinates, where all coords given
    // are for the same frequency - maxHops determines how many jumps we are allowed to
    // take
    let allAntinodes maxHops antennae =
        antennae
        |> List.allPairs antennae
        // Allowing multiple hops makes the antenna locations antinodes - this doesn't fall
        // out of my implementation so simply neglect to filter out the false pair
        |> List.filter (fun (a, b) -> maxHops > 1 || a <> b)
        |> List.collect (fun (a, b) -> getAntinodes maxHops a b)
        |> Set.ofList

    let solve withHops input =
        let antennae, bounds = parse input

        // The max is definitely overkill. Each pairing could determine its own maxHops that
        // will get them to the edge of the map in both directions...
        let maxHops = if withHops then max bounds.x bounds.y else 1

        antennae
        |> List.map (allAntinodes maxHops)
        |> Set.unionMany
        |> Set.filter (inbounds bounds)
        |> Set.count

    interface Day with
        member this.DayName = "08"
        member this.answer1 = "276"
        member this.answer2 = "991"

        member this.part1() = solve false input |> string
        member this.part2() = solve true input |> string
