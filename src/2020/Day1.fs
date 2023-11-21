module Year2020.Day1

open System.IO
open Scaffold.Handlers

let parse = 
    File.ReadAllLines
    >> Array.toList
    >> List.map int

let solveSilver (input: int list) =
    seq { for i in input do
            for j in input do
                if i <> j && i + j = 2020 then i * j }
    |> Seq.head
    |> string

let solveGold (input: int list) =
    seq { for i in input do
            for j in input do
                for k in input do
                    if i <> j && j <> k && k <> i && i + j + k = 2020 then i * j * k }
    |> Seq.head
    |> string 

let Solver = chainFileHandler parse solveSilver solveGold