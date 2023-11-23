module Year2020.Day4

open System.IO
open System.Text.RegularExpressions
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Util.Patterns
open Scaffold.Extensions

let parse path =
    let reg = Regex @"([a-z]{3}):(\S+)"
    let parsePassport input =
        reg.Matches input
        |> Seq.map (_.Groups >> Seq.tail >> Seq.map _.Value >> Seq.toPair)
        |> Map.ofSeq

    File.ReadAllText path
    |> _.Split("\n\n")
    |> Array.toList
    |> List.map parsePassport

let solveSilver input =
    let checkPassport pp =
        ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"]
        |> List.forall (fun k -> Map.containsKey k pp)
    
    input
    |> List.filter checkPassport
    |> List.length  
    |> sprintf "%A"

let solveGold input =
    let checkPassport pp =
        [
            "byr", function Int32 n when n >= 1920 && n <= 2002 -> true | _ -> false
            "iyr", function Int32 n when n >= 2010 && n <= 2020 -> true | _ -> false
            "eyr", function Int32 n when n >= 2020 && n <= 2030 -> true | _ -> false
            "hcl", (Regex @"^#[0-9a-f]{6}$").IsMatch
            "ecl", fun col -> Set ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"] |> Set.contains col
            "pid", (Regex @"^\d{9}$").IsMatch
            "hgt", function | RegGroups @"^(\d+)(cm|in)$" [Int32 n; unit] when (unit = "cm" && n >= 150 && n <= 193) || (unit = "in" && n >= 59 && n <= 76) -> true | _ -> false
        ]
        |> List.forall (fun (key, pred) -> Map.containsKey key pp && pred pp[key])
    
    input
    |> List.filter checkPassport
    |> List.length  
    |> sprintf "%A"

[<Solution("2020", "4", "Passport Processing")>]
let Solver = chainFileHandler parse solveSilver solveGold