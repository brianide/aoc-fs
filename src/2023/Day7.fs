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

let solve jokerMode =
    let pullJokers hand =
        let jokers, cards = hand |> Seq.splitWith (fun c -> c = 'J')
        List.length jokers, cards

    let classifyHand jokers freqs =
        freqs
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

    let processHand hand =
        let (jokers, cards) = if jokerMode then pullJokers hand else (0, hand)

        cards
        |> Seq.frequencies
        |> Seq.sortByDescending snd
        |> Seq.toList
        |> classifyHand jokers
        |> fun cat -> hand, cat

    let cardStrength = function
    | 'A' -> 14
    | 'K' -> 13
    | 'Q' -> 12
    | 'T' -> 10
    | 'J' -> if jokerMode then 1 else 11
    | c when c >= '2' && c <= '9' -> int c - int '0'
    | c -> failwithf "Invalid card: %A" c

    File.ReadAllLines
    >> Seq.map (_.Split(' ') >> Seq.toPair >> fun (hand, bid) -> Seq.toList hand, int bid)
    >> Seq.map (fun (hand, bid) -> processHand hand, bid)
    >> Seq.sortBy (fun ((hand, cat), _) -> cat, List.map cardStrength hand)
    >> Seq.mapi (fun i (_, bid) -> (i + 1) * bid)
    >> Seq.sum
    >> sprintf "%A"

[<Solution("2023", "7", "Camel Cards")>]
let Solver = simpleFileHandler (solve false) (solve true)