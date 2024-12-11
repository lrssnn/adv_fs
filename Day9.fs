module adv_fs.Day9

open Puz.Input

type FileBlock = { label: int option; length: int }

type Day9() =
    let input = readText "input/input09.txt"

    let testInput = "2333133121414131402"

    /// Assumes arr is 1 or 2 elements exactly. Treats the 1st as a file, and the 2nd if provided as an empty
    let handlePair (identifier, (arr: int array)) =
        if arr.Length = 1 then
            [ { label = Some(identifier)
                length = arr[0] } ]
        else
            [ { label = Some(identifier)
                length = arr[0] }
              { label = None; length = arr[1] } ]

    // Take the input string and turn it into a list of FileBlocks representing the disk
    let expand input =
        input
        |> Seq.map (fun c -> int (string c))
        |> Seq.chunkBySize 2
        |> Seq.indexed
        |> Seq.collect handlePair
        |> List.ofSeq
        |> List.filter (fun block -> block.length > 0)

    // Move the last block of the disk to the targetIndex provided
    // Will split the block if required
    let moveBlockTo fileBlocks targetIndex =
        let lastIndex = List.length fileBlocks - 1
        let blockToMove = List.last fileBlocks
        let targetBlock = List.item targetIndex fileBlocks

        if blockToMove.length = targetBlock.length then
            // Same length - blockToMove turns into an empty block, targetBlock turns into blockToMove
            fileBlocks
            |> List.indexed
            |> List.choose (fun (i, e) ->
                match i with
                | x when x = targetIndex -> Some(blockToMove)
                | x when x = lastIndex -> None
                | _ -> Some(e))
        else if blockToMove.length > targetBlock.length then
            // Replace the entire target block with as much blockToMove as we can, keeping the rest at the end
            fileBlocks
            |> List.indexed
            |> List.map (fun (i, e) ->
                match i with
                | x when x = targetIndex ->
                    { targetBlock with
                        label = blockToMove.label }
                | x when x = lastIndex ->
                    { blockToMove with
                        length = blockToMove.length - targetBlock.length }
                | _ -> e)
        else
            // Put the blockToMove in front of the targetBlock, targetBlock reduces in size
            fileBlocks
            |> List.indexed
            |> List.collect (fun (i, e) ->
                match i with
                | x when x = targetIndex ->
                    [ blockToMove
                      { targetBlock with
                          length = targetBlock.length - blockToMove.length } ]
                | x when x = lastIndex -> []
                | _ -> [ e ])


    // Apply a single defrag operation by attempting to move block with id idToMove as far left as it can go
    // without splitting the block. Will recursively request to defrag the next lowest index until we run out of files
    let rec defrag fileBlocks idToMove =
        let optIFileToMove =
            fileBlocks
            |> List.tryFindIndexBack (fun file -> file.label.IsSome && file.label.Value = idToMove)

        match optIFileToMove with
        | None -> fileBlocks
        | Some(iFileToMove) ->
            let fileToMove = fileBlocks.Item iFileToMove

            let optIFreeBlock =
                fileBlocks
                |> List.tryFindIndex (fun block -> block.label.IsNone && block.length >= fileToMove.length)

            let nextIdToMove = fileToMove.label.Value - 1

            match optIFreeBlock with
            | None -> defrag fileBlocks nextIdToMove
            | Some(iFreeBlock) when iFreeBlock >= iFileToMove -> defrag fileBlocks nextIdToMove
            | Some(iFreeBlock) ->
                let freeBlock = fileBlocks.Item iFreeBlock

                let newBlocks =
                    fileBlocks
                    |> List.indexed
                    |> List.collect (fun (i, block) ->

                        match i with
                        | x when x = iFreeBlock ->
                            if freeBlock.length = fileToMove.length then
                                [ fileToMove ]
                            else
                                [ fileToMove
                                  { freeBlock with
                                      length = freeBlock.length - fileToMove.length } ]
                        | x when x = iFileToMove -> [ { fileToMove with label = None } ]
                        | _ -> [ block ])

                defrag newBlocks nextIdToMove

    let rec moveBlocks fileBlocks =
        let firstFree = List.tryFindIndex (fun b -> b.label.IsNone) fileBlocks

        match firstFree with
        | None -> fileBlocks
        | Some(index) -> moveBlocks (moveBlockTo fileBlocks index)

    let toIntList fileBlocks =
        fileBlocks
        |> List.collect (fun fileBlock ->
            match fileBlock.label with
            | Some(id) -> int64 id
            | None -> int64 0
            |> Seq.replicate fileBlock.length
            |> Seq.toList)

    let checksum fileBlocks =
        fileBlocks
        |> toIntList
        |> Seq.indexed
        |> Seq.sumBy (fun (i, v) -> (int64 i) * v)

    interface Day with
        member this.DayName = "09"
        member this.answer1 = "6283170117911"
        member this.answer2 = "6307653242596"

        member this.part1() = "Slow"
        // let files = expand input
        // let compressed = moveBlocks files
        // string (checksum compressed)

        member this.part2() = "Slow"
// let files = expand input
//
// let maxLabel =
//     (files |> List.maxBy (fun b -> b.label |> Option.defaultValue -1)).label.Value
//
// let compressed = defrag files maxLabel
// string (checksum compressed)
