module adv_fs.Day5

open System
open Puz.Input
open Puz.Convenience
open FSharpx.String

type Day5() =
    let input = readText "input/input05.txt"

    let parseRules input =
        input
        |> splitString [| "\n" |] StringSplitOptions.None
        |> Array.map (splitOnce '|')
        |> Array.map (fun (a, b) -> (int a, int b))
        |> List.ofArray

    let parseUpdates input =
        input
        |> splitString [| "\n" |] StringSplitOptions.None
        |> Array.map (splitChar [| ',' |])
        |> Array.map (fun line -> (Array.map int) line |> List.ofArray)
        |> List.ofArray

    let parse input =
        let (top, bottom) = splitOnceStr "\n\n" input
        (parseRules top, parseUpdates bottom)

    let indexOf list target =
        List.findIndex (fun e -> e = target) list

    // Returns true if the update is valid according to the rules provided
    let updateIsValid rules update =
        // Relevant rules are those in which both items appear in the update list
        let relevantRules =
            rules
            |> List.filter (fun (a, b) -> List.contains a update && List.contains b update)

        // The rule is valid if every relevant rule is satisfied
        relevantRules |> List.forall (fun (a, b) -> indexOf update a < indexOf update b)

    let middleItem list = List.item ((List.length list) / 2) list

    let solve input =
        let (rules, updates) = parse input

        updates |> List.filter (updateIsValid rules) |> List.sumBy middleItem

    // Returns true if 'item' can be added to the list, based on the rules provided
    let canBePlaced rules item =
        rules
        // Get all the items in the rules list which appear in the second part of the rule
        // That is - get the "After" part of the rule - these are the numbers which we cannot place yet
        // (in other words, the numbers which still have an unfulfilled prerequisite)
        |> List.map snd
        // If the item does not appear in that list, it can be placed
        |> List.forall (fun b -> not (b = item))

    // Builds a list like [acc] :: [toBePlaced ordered such that it satisfies all of 'rules']
    let rec order toBePlaced rules acc =
        // Nothing left to be placed, reverse acc (because we have been building it backwards
        if List.length toBePlaced = 0 then
            List.rev acc
        else
            // Build an ordering by finding an item which can be attached without breaking any rules
            // Find the rules which apply to the remainder of the list
            let relevantRules =
                rules
                |> List.filter (fun (a, b) -> List.contains a toBePlaced && List.contains b toBePlaced)

            // The next item is the single item which is not required to be after any of the other numbers
            // We happen to know there is exactly one, finding the first is good enough
            let next = toBePlaced |> List.find (canBePlaced relevantRules)

            // Recurse after moving 'next' out of the toBePlaced list into the acc
            order (List.except [ next ] toBePlaced) relevantRules (next :: acc)

    let solve2 input =
        let (rules, updates) = parse input

        updates
        |> List.filter (fun update -> not (updateIsValid rules update))
        |> List.map (fun update -> order update rules [])
        |> List.sumBy middleItem

    interface Day with
        member this.DayName = "05"
        member this.answer1 = "6384"
        member this.answer2 = "5353"

        member this.part1() = string (solve input)

        member this.part2() = string (solve2 input)
