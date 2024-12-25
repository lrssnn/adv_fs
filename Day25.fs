module adv_fs.Day25

open System
open Puz.Input
open FSharpx.String

type Key = int array
type Lock = int array

type Item =
    | Key of Key
    | Lock of Lock

type Day25() =
    let input = readText "input/input25.txt"

    let readRows (rows: char list list) : int array =
        rows
        |> List.map (fun row -> row |> List.findIndexBack (fun e -> e = '#'))
        |> Array.ofList

    let parseLock str =
        Lock(str |> readAsCharListList |> List.transpose |> readRows)

    let parseKey str =
        Key(str |> readAsCharListList |> List.transpose |> List.map List.rev |> readRows)

    let parseItem (item: string) =
        if item[0] = '#' then parseLock item else parseKey item

    let parse input =
        let ls, ks =
            input
            |> splitString [| "\n\n" |] StringSplitOptions.None
            |> Array.map parseItem
            |> Array.partition (fun item ->
                match item with
                | Lock _ -> false
                | Key _ -> true)

        // This seems like a bad way to do this
        (ks
         |> Array.map (fun item ->
             match item with
             | Lock l -> l
             | _ -> failwith "impossible")),
        (ls
         |> Array.map (fun item ->
             match item with
             | Key l -> l
             | Lock _ -> failwith "impossible"))

    let couldFit (lock: Lock) (key: Key) =
        [ 0..4 ] |> List.forall (fun i -> key[i] + lock[i] <= 5)

    interface Day with
        member this.DayName = "25"
        member this.answer1 = "3301"
        member this.answer2 = "Free!"

        member this.part1() =
            let locks, keys = parse input

            locks
            |> Array.sumBy (fun lock -> keys |> Array.where (couldFit lock) |> Array.length)
            |> string

        member this.part2() = "Free!"
