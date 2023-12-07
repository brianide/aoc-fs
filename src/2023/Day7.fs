module Year2023.Day7

open System.IO
open Scaffold.Attributes
open Scaffold.Handlers
open Scaffold.Extensions

type HandType =
| HighCard
| OnePair
| TwoPair
| ThreeKind
| FullHouse
| FourKind
| FiveKind

let classifyHand jokers cards =
    cards
    |> List.mapi (fun i (_, v) -> if i = 0 then v + jokers else v)
    |> function
    | 5 :: _ -> FiveKind
    | 4 :: _ -> FourKind
    | 3 :: 2 :: _ -> FullHouse
    | 3 :: _ -> ThreeKind
    | 2 :: 2 :: _ -> TwoPair
    | 2 :: _ -> OnePair
    | 1 :: _ -> HighCard
    | [] when jokers = 5 -> FiveKind
    | x -> failwithf "Invalid hand: %A" x

let cardStrength jokerMode = function
| 'A' -> 14
| 'K' -> 13
| 'Q' -> 12
| 'T' -> 10
| 'J' -> if jokerMode then 1 else 11
| c when c >= '2' && c <= '9' -> int c - int '0'
| c -> failwithf "Invalid card: %A" c

let solveSilver path =
    File.ReadAllLines path
    |> Seq.map (_.Split(' ') >> Seq.toPair >> fun (hand, bid) -> Seq.map (cardStrength false) hand |> Seq.toList, int bid)
    |> Seq.map (fun (hand, bid) -> hand, hand |> Seq.frequencies |> Seq.sortByDescending snd |> Seq.toList |> classifyHand 0, bid)
    |> Seq.sortBy (fun (hand, cat, _) -> cat, hand)
    |> Seq.mapi (fun i (_, _, bid) -> (i + 1) * bid)
    |> Seq.sum
    |> sprintf "%A"

let solveGold path =
    let pullJokers hand =
        let jokers, cards = hand |> Seq.splitWith (fun c -> c = 1)
        List.length jokers, cards

    File.ReadAllLines path
    |> Seq.map (_.Split(' ') >> Seq.toPair >> fun (hand, bid) -> Seq.map (cardStrength true) hand |> Seq.toList, int bid)
    |> Seq.map (fun (hand, bid) -> hand, pullJokers hand, bid)
    |> Seq.map (fun (hand, (jokers, cards), bid) -> hand, cards |> Seq.frequencies |> Seq.sortByDescending snd |> Seq.toList |> classifyHand jokers, bid)
    |> Seq.sortBy (fun (hand, cat, _) -> cat, hand)
    |> Seq.mapi (fun i (_, _, bid) -> (i + 1) * bid)
    |> Seq.sum
    |> sprintf "%A"

[<Solution("2023", "7", "Camel Cards")>]
let Solver = simpleFileHandler solveSilver solveGold