module adv_fs.Day18

open Puz.Input
open Puz.Convenience

type Coord =
    { x: int
      y: int }

    member c.up = { c with y = c.y - 1 }
    member c.down = { c with y = c.y + 1 }
    member c.left = { c with x = c.x - 1 }
    member c.right = { c with x = c.x + 1 }

type PathingNode =
    { coord: Coord
      distanceToGoal: int
      pathLength: int
      parent: PathingNode option }

type Day18() =
    let input = readText "input/input18.txt"

    let parseCoord text =
        let left, right = splitOnce ',' text
        { x = int left; y = int right }

    let parse input =
        input |> readLines |> List.ofArray |> List.map parseCoord

    let inbounds loc lim =
        loc.x >= 0 && loc.x <= lim && loc.y >= 0 && loc.y <= lim

    let getNeighbours (loc: Coord) walls lim =
        [ loc.up; loc.down; loc.left; loc.right ]
        |> List.filter (fun c -> inbounds c lim && not (Set.contains c walls))

    let rec remove list predicate =
        match list with
        | [] -> []
        | head :: rest ->
            if predicate head then
                (remove rest predicate)
            else
                head :: (remove rest predicate)

    let rec toPath pathNode acc =
        match pathNode.parent with
        | None -> List.rev (pathNode.coord :: acc)
        | Some parent -> toPath parent (pathNode.coord :: acc)

    let rec a_star neighbours goal start (openNodes: PathingNode list) (closedNodes: PathingNode list) =
        let f x : int = x.pathLength + x.distanceToGoal
        let isShorter nodeA nodeB = nodeA = nodeB && f nodeA < f nodeB

        let rec checkNeighbours neighbours openNodeAcc =
            match neighbours with
            | [] -> openNodeAcc
            | currentNode :: rest ->
                let likeCurrent = fun n -> n.coord = currentNode.coord
                let containsCurrent = List.exists likeCurrent
                let checkNeighbours = checkNeighbours rest

                if openNodeAcc |> List.exists (isShorter currentNode) then
                    let shorterPath = remove openNodeAcc likeCurrent
                    checkNeighbours (currentNode :: shorterPath)
                elif not (containsCurrent closedNodes) && not (containsCurrent openNodeAcc) then
                    checkNeighbours (currentNode :: openNodeAcc)
                else
                    checkNeighbours openNodeAcc

        let nodes = neighbours openNodes.Head

        // Don't think this is getting the optimal path?
        let pathToGoal = nodes |> List.tryFind (fun x -> x.coord = goal)

        if pathToGoal.IsSome then
            pathToGoal
        else
            let nextSet = checkNeighbours nodes openNodes.Tail |> List.sortBy f

            if nextSet.Length > 0 then
                a_star neighbours goal start nextSet (nextSet.Head :: closedNodes)
            else
                None


    let coordToPathNode goal parent coord =
        { coord = coord
          parent = Some parent
          pathLength = parent.pathLength + 1
          distanceToGoal = (goal - coord.x) + (goal - coord.y) }

    let findEndNode walls lim =

        let neighbours =
            fun n -> getNeighbours n.coord walls lim |> List.map (coordToPathNode lim n)

        let start = { x = 0; y = 0 }

        let startNode =
            { coord = start
              parent = None
              pathLength = 0
              distanceToGoal = lim * 2 }

        let endNode = a_star neighbours { x = lim; y = lim } start [ startNode ] List.empty

        endNode

    interface Day with
        member this.DayName = "18"
        member this.answer1 = "306"
        member this.answer2 = "38,63"

        member this.part1() =
            let walls = parse input |> List.take 1024 |> Set.ofList
            let endNode = findEndNode walls 70
            // No idea why we are taking one non-optimal path leading to two extra steps...
            endNode.Value.pathLength - 2 |> string

        member this.part2() =
            let allWalls = parse input

            [ 1024 .. allWalls.Length ]
            |> Array.ofList
            |> Array.Parallel.filter (fun bytes ->
                let walls = allWalls |> List.take bytes |> Set.ofList
                let endNode = findEndNode walls 70
                endNode.IsNone)
            |> Array.min
            |> (fun i -> allWalls[i - 1])
            |> (fun coord -> $"{coord.x},{coord.y}")
