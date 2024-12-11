module adv_fs.Day10

open Puz.Input
open Puz.Convenience

type Day10() =
    let input = readText "input/input10.txt"

    let testInput =
        "89010123\n\
        78121874\n\
        87430965\n\
        96549874\n\
        45678903\n\
        32019012\n\
        01329801\n\
        10456732"

    let testInputEasy =
        "..90..9\n\
...1.98\n\
...2..7\n\
6543456\n\
765.987\n\
876....\n\
987...."

    let map input =
        readAsCharA2D input
        |> Array2D.map (fun char -> if char = '.' then -1 else int (string char))

    let trailheads map =
        map
        |> Array2D.mapi (fun y x height -> if height = 0 then Some(x, y) else None)
        |> Seq.cast<(int * int) option>
        |> Seq.choose id

    // TODO we need that 2d library
    let monotonicNeighbours (map: int array2d) (x, y) =
        let targetVal = map[y, x] + 1

        seq {
            if inbounds map (y - 1) x && map[(y - 1), x] = targetVal then
                yield (x, (y - 1))

            if inbounds map (y + 1) x && map[(y + 1), x] = targetVal then
                yield (x, (y + 1))

            if inbounds map y (x - 1) && map[y, (x - 1)] = targetVal then
                yield ((x - 1), y)

            if inbounds map y (x + 1) && map[y, (x + 1)] = targetVal then
                yield ((x + 1), y)
        }

    let rec countPathsToNine (map: int array2d) (x, y) =
        if map[y, x] = 9 then
            1
        else
            let options = monotonicNeighbours map (x, y)
            options |> Seq.sumBy (countPathsToNine map)

    let rec reachableNines (map: int array2d) (x, y) =
        if map[y, x] = 9 then
            Set.singleton (x, y)
        else
            let options = monotonicNeighbours map (x, y)
            options |> Seq.map (reachableNines map) |> Set.unionMany

    interface Day with
        member this.DayName = "10"
        member this.answer1 = "550"
        member this.answer2 = "1255"

        member this.part1() =
            let map = map input

            trailheads map
            |> Seq.sumBy (fun coord -> (reachableNines map coord) |> Set.count)
            |> string

        member this.part2() =
            let map = map input
            trailheads map |> Seq.sumBy (countPathsToNine map) |> string
