(* Backtracking algorithm to solve sudoku.

   y: row index
   x: col index *)


// Alias type
type Sudoku = int list list


let sample =
    [ [0;0;0;  2;6;0;  7;0;1]
      [6;8;0;  0;7;0;  0;9;0]
      [1;9;0;  0;0;4;  5;0;0]

      [8;2;0;  1;0;0;  0;4;0]
      [0;0;4;  6;0;2;  9;0;0]
      [0;5;0;  0;0;3;  0;2;8]

      [0;0;9;  3;0;0;  0;7;4]
      [0;4;0;  0;5;0;  0;3;6]
      [7;0;3;  0;1;8;  0;0;0] ]


let print sudoku =
    List.iteri (fun y row -> 
        if y % 3 = 0 then printfn " +-------+-------+-------+"
        List.iteri (fun x n -> 
            if x % 3 = 0 then printf " |"
            if n = 0 then printf " ." else printf $" {n}"
        ) row
        printfn " |"
    ) sudoku
    printfn " +-------+-------+-------+"


let possibleRow y n (sudoku: Sudoku) =
    List.contains n sudoku.[y] |> not


let possibleCol x n sudoku =
    sudoku
    |> List.exists (fun (row: int list) -> row.[x] = n)
    |> not


let possibleSquare y x n (sudoku: Sudoku) =
    let y0 = y / 3 * 3
    let x0 = x / 3 * 3
    [y0 .. y0 + 2]
    |> List.map (fun i -> sudoku.[i].[x0 .. x0 + 2])
    |> List.reduce List.append
    |> List.contains n
    |> not


let possible y x n sudoku =
    (possibleRow y n sudoku) && (possibleCol x n sudoku) && (possibleSquare y x n sudoku)


let insertNumber y x n (sudoku: Sudoku) =
    let replace idx v ls =
        List.mapi (fun i x -> if i = idx then v else x) ls
    let newRow = replace x n sudoku.[y]
    replace y newRow sudoku


let rec solvePosition pos sudoku contf =
    let rec solveSingle y x n =
        if n = 10 then
            contf()
        else if possible y x n sudoku then
            let newSudoku = insertNumber y x n sudoku
            solvePosition (pos + 1) newSudoku (fun () -> solveSingle y x (n + 1))
        else
            solveSingle y x (n + 1)

    if pos = 81 then
        Some sudoku
    else
        let y = pos / 9
        let x = pos % 9
        if sudoku.[y].[x] = 0 then
            solveSingle y x 1
        else
            solvePosition (pos + 1) sudoku contf


let solve sudoku =
    printfn "INPUT:"
    print sudoku
    printfn "OUTPUT:"
    match solvePosition 0 sudoku (fun () -> None) with
    | Some solution -> print solution
    | None          -> printfn "This sudoku does not have any solution."


solve sample


(*
INPUT:
+-------+-------+-------+
| . . . | 2 6 . | 7 . 1 |
| 6 8 . | . 7 . | . 9 . |
| 1 9 . | . . 4 | 5 . . |
+-------+-------+-------+
| 8 2 . | 1 . . | . 4 . |
| . . 4 | 6 . 2 | 9 . . |
| . 5 . | . . 3 | . 2 8 |
+-------+-------+-------+
| . . 9 | 3 . . | . 7 4 |
| . 4 . | . 5 . | . 3 6 |
| 7 . 3 | . 1 8 | . . . |
+-------+-------+-------+
OUTPUT:
+-------+-------+-------+
| 4 3 5 | 2 6 9 | 7 8 1 |
| 6 8 2 | 5 7 1 | 4 9 3 |
| 1 9 7 | 8 3 4 | 5 6 2 |
+-------+-------+-------+
| 8 2 6 | 1 9 5 | 3 4 7 |
| 3 7 4 | 6 8 2 | 9 1 5 |
| 9 5 1 | 7 4 3 | 6 2 8 |
+-------+-------+-------+
| 5 1 9 | 3 2 6 | 8 7 4 |
| 2 4 8 | 9 5 7 | 1 3 6 |
| 7 6 3 | 4 1 8 | 2 5 9 |
+-------+-------+-------+
*)