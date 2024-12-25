module adv_fs.Day20

open Puz.Input

type Coord = { x: int; y: int }
type Node = { loc: Coord; cost: int }

type Day20() =
    let input = readText "input/input20.txt"

    let testInput =
        "###############\n\
                    #...#...#.....#\n\
                    #.#.#.#.#.###.#\n\
                    #S#...#.#.#...#\n\
                    #######.#.#.###\n\
                    #######.#.#...#\n\
                    #######.#.###.#\n\
                    ###..E#...#...#\n\
                    ###.#######.###\n\
                    #...###...#...#\n\
                    #.#####.#.###.#\n\
                    #.#...#.#.#...#\n\
                    #.#.#.#.#.#.###\n\
                    #...#...#...###\n\
                    ###############"

    let findChar charGrid target =
        let y = charGrid |> List.findIndex (List.exists (fun tile -> tile = target))
        let x = charGrid[y] |> List.findIndex (fun tile -> tile = target)
        { x = x; y = y }

    let canGoTo charGrid candidate seekingWall =
        candidate.y >= 0
        && candidate.y < List.length charGrid
        && candidate.x >= 0
        && candidate.x < List.length charGrid[0]
        && if seekingWall then
               charGrid[candidate.y][candidate.x] = '#'
           else
               not (charGrid[candidate.y][candidate.x] = '#')

    let allowedMoves charGrid at from seekingWall =
        seq {
            let up = { at with y = at.y - 1 }

            if canGoTo charGrid up seekingWall && not (up = from) then
                printfn "Can go up"
                yield up

            let down = { at with y = at.y + 1 }

            if canGoTo charGrid down seekingWall && not (down = from) then
                printfn "Can go down"
                yield down

            let left = { at with x = at.x - 1 }

            if canGoTo charGrid left seekingWall && not (left = from) then
                printfn "Can go left"
                yield left

            let right = { at with x = at.x + 1 }

            if canGoTo charGrid right seekingWall && not (right = from) then
                printfn "Can go right"
                yield right
        }
        |> List.ofSeq

    let singleAllowedMove charGrid at from =
        let up = { at with y = at.y - 1 }

        if canGoTo charGrid up false && not (up = from) then
            printfn "Can go up"
            up
        else

            let down = { at with y = at.y + 1 }

            if canGoTo charGrid down false && not (down = from) then
                printfn "Can go down"
                down
            else

                let left = { at with x = at.x - 1 }

                if canGoTo charGrid left false && not (left = from) then
                    printfn "Can go left"
                    left
                else

                    let right = { at with x = at.x + 1 }

                    if canGoTo charGrid right false && not (right = from) then
                        printfn "Can go right"
                        right
                    else
                        failwith "Got stuck..."


    let rec buildPath path from at goal charGrid =
        printfn $"pathfinder at ({at.x}, {at.y}) came from ({from.x},{from.y})"

        if at = goal then
            path
        else
            let move = singleAllowedMove charGrid at from
            buildPath (move :: path) at move goal charGrid

    let findPath charGrid =
        let start = findChar charGrid 'S'
        let goal = findChar charGrid 'E'
        let coordPath = buildPath [ start ] start start goal charGrid

        // Run through the path assigning the costs - the coord path is in reverse order
        coordPath
        |> List.indexed
        |> List.map (fun (cost, coord) -> { loc = coord; cost = cost })
        |> List.rev

    let parse input =
        // In this challenge, the pathfinding is effectively a part of parsing. Simply parse into
        // the path itself, a list of coordinates associated with their distance to the end
        let charGrid = input |> readAsCharListList
        (charGrid, findPath charGrid)

    let findShortcutsFrom charGrid path fromNode =
        printfn $"Finding shortcuts from ({fromNode.loc.x}, {fromNode.loc.y})"
        // Find any neighbouring walls - step inside and then look for where we could go
        let walls = allowedMoves charGrid fromNode.loc fromNode.loc true

        let destinations =
            walls
            |> List.collect (fun wall -> allowedMoves charGrid wall fromNode.loc false)

        let destNodes =
            destinations
            |> List.map (fun coord -> path |> List.find (fun node -> node.loc.x = coord.x && node.loc.y = coord.y))

        let costs =
            destNodes |> List.map (fun destination -> fromNode.cost - destination.cost - 2)

        costs

    let findAllShortcuts charGrid path =
        path |> List.collect (findShortcutsFrom charGrid path)

    interface Day with
        member this.DayName = "20"
        member this.answer1 = "1452"
        member this.answer2 = "?"

        member this.part1() =
            let (charGrid, path) = parse input
            let shortcuts = findAllShortcuts charGrid path
            let groups = shortcuts |> List.countBy id |> List.sortBy fst

            groups
            |> List.filter (fun (cost, count) -> cost >= 100)
            |> List.sumBy snd
            |> string

        member this.part2() = "!"
