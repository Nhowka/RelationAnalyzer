open System.IO
open System

let (|Simple|Aliased|Neither|) (c : string) = 
    match c.Split ' ' with
    | [| a |] -> Simple(Some(a, a))
    | [| a; b |] -> Aliased(Some(b, a))
    | _ -> Neither(None : (string * string) option)

type Info = 
    | Primary of (string * string)
    | Relation of ((string * string) * (string * string))

let extractor lines = 
    let fromWhere (line : string) = 
        let (from, where) = 
            let notWhere = ((<>) "WHERE")
            line.Split ' '
            |> Seq.skipWhile ((<>) "FROM")
            |> (fun s -> 
            let r = s |> Seq.skip 1
            let f = r |> Seq.takeWhile notWhere
            (f, 
             r
             |> Seq.skipWhile notWhere
             |> Seq.skip 1))
        
        let (fromEntries, whereEntries) = 
            ((from |> String.concat " ").Split([| ", " |], StringSplitOptions.RemoveEmptyEntries), 
             (where |> String.concat " ").Split([| " AND " |], StringSplitOptions.RemoveEmptyEntries))
        
        let tables = 
            fromEntries
            |> Seq.choose (function 
                   | Simple c | Aliased c | Neither c -> c)
            |> Map.ofSeq
        
        let (|Relation|Primary|) (c : string) = 
            let toKey (s : string) = 
                match s.Split '.' with
                | [| a; b |] -> (tables |> Map.find a, b)
                | [| a |] -> (tables |> Map.pick (fun k _ -> Some k), a)
                | _ -> failwith "Not sure of the full name"
            match c.Split([| " = " |], StringSplitOptions.RemoveEmptyEntries) with
            | [| a; "?" |] -> Primary(toKey a)
            | [| a; b |] -> Relation(toKey a, toKey b)
            | _ -> failwith "Invalid format"
        
        let expandedWhere = 
            whereEntries |> Seq.map (function 
                                | Primary n -> Primary n
                                | Relation n -> Relation n)
        
        let primaries = 
            expandedWhere |> Seq.choose (function 
                                 | Info.Primary c -> Some c
                                 | _ -> None)
        
        let rec getRel relList rels = 
            match relList with
            | [] -> rels
            | (a, b) :: r -> 
                match rels |> Map.tryFind a, rels |> Map.tryFind b with
                | Some a', Some b' -> 
                    rels
                    |> Map.add b (a :: a')
                    |> Map.add a (b :: b')
                    |> getRel r
                | None, Some b' -> 
                    rels
                    |> Map.add b [ a ]
                    |> Map.add a (b :: b')
                    |> getRel r
                | Some a', None -> 
                    rels
                    |> Map.add b (a :: a')
                    |> Map.add a [ b ]
                    |> getRel r
                | None, None -> 
                    rels
                    |> Map.add b [ a ]
                    |> Map.add a [ b ]
                    |> getRel r
        
        let relList = 
            expandedWhere
            |> Seq.choose (function 
                   | Info.Relation c -> Some c
                   | _ -> None)
            |> Seq.toList
        
        let relations = getRel relList Map.empty |> Map.toSeq
        primaries, relations
    
    let join l = 
        let rec joiner res l = 
            match l, res with
            | a :: r, [] -> joiner [ a ] r
            | (a, a') :: r, (l, l') :: r' when a = l -> joiner ((a, a' @ l') :: r') r
            | a :: r, r' -> joiner (a :: r') r
            | [], _ -> res
        joiner [] l
    
    let (primaries, relations) = 
        lines
        |> Array.map fromWhere
        |> Array.unzip
        |> (fun (a, b) -> 
        a
        |> Seq.collect id
        |> Seq.countBy id
        |> Seq.sortByDescending snd
        |> Seq.sortBy (fst>>fst)
        |> Seq.map (fun ((table, col), n) -> sprintf "Table %s had %i references to column %s" table n col), 
        b
        |> Seq.collect id
        |> Seq.sortBy fst
        |> Seq.toList
        |> join
        |> Seq.map 
               (fun ((t1, c1), b) -> 
               let baseMessage = sprintf "Table %s, column %s had" t1 c1
               
               let counting = 
                   b
                   |> Seq.countBy id
                   |> Seq.sortByDescending snd
               baseMessage + (counting 
               |> Seq.map (fun ((t2, c2), n) -> sprintf "\n\t%i references to table %s, column %s" n t2 c2)
                                                |> String.concat ","))
        )
    
    //|> Seq.map (fun ((t1, c1),(t2,c2), n) -> sprintf "Table %s had %i references to column %s" table n col)
    primaries, relations

[<EntryPoint>]
let main argv = 
    argv.[0]
    |> File.ReadAllLines
    |> extractor
    |> fun (p, r) -> 
        Seq.iter (printfn "%s") p
        Seq.iter (printfn "%s") r
    0 // return an integer exit code
