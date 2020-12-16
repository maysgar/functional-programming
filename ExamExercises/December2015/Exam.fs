namespace December2015

module Exam = 

    type Appliance = string
    type Usage = Appliance*int

    type Price = int
    type Tariff = Map<Appliance, Price>

    let ad1 = ("washing machine", 2) : Usage
    let ad2 = ("coffee machine", 1) : Usage
    let ad3 = ("dishwasher", 2) : Usage
    let ats = [ad1; ad2; ad3; ad1; ad2] : Usage list


    let inv(usageList : Usage list) : bool =
        
        let rec recInv(usage, usageList) = 
            match usage, usageList with
                | (_, []) -> true
                | ((machine, time), _) when time < 0 -> false
                | (usage, head::tail) -> recInv(usage, tail) 

        let head = usageList.Head 
        recInv(head, usageList)

    let rec g2 f h n x =
        match n with 
        | _ when n < 0 -> failwith "negative not allowed"
        | 0 -> x
        | n -> g2 h f (n-1) (f x)
    

    let f2 f p sq = seq { for x in sq do if p x then yield f x }
   

    // PROBLEM 3
    type Name = string
    type Flow = int
    type River = R of Name * Flow * Tributaries
    and Tributaries = River list

    // 3.1
    let riv4 = R("R4", 2, [])
    let riv1 = R("R1", 5, [])
    let riv2 = R("R2", 15, [riv4])
    let riv3 = R("R3", 8, [])

    let riv = R("R", 10, [riv1; riv2; riv3])


    // 3.2
    let contains n (R (r, flow, tribs)) = 
        if n = r then true
        else
            let mutable value = false
            for R (name, _, _) in tribs do
                match name with
                | _ when name = n -> value <- true
                | _ -> ()
            value

    // 3.3
    let rec allNames (R (rName, flow, tributaries)) =
        match tributaries with
            | ([]) -> []
            | (R (name, _, _)::tail) -> name::allNames (R (rName, flow, tail))

    
    // 3.4
    let rec getFlowTribs tribs =
        match tribs with
            | [] -> 0
            | (R (rName, flow, tributaries)::tail) -> flow + getFlowTribs tributaries + getFlowTribs tail


    let totalFlow (R (rName, flow, tributaries) : River ) : Flow =
        flow + getFlowTribs tributaries

    
    // 3.5
    let rec mainSource (R (rName, flow, tributaries) : River ) : Name*Flow =
        match tributaries with
            | [] -> rName, flow
            | (R (n, f, t))::tail -> 
                let rAux, sAux = mainSource (R (n, f, t))
                if sAux > flow then mainSource (R (rAux, sAux, tail))
                else mainSource (R (rName, flow, tail))


    // 3.6
    let insert (R (rNameT, flowT, tribsT)) (R (rName, flow, tributaries)) =
        R (rName, flow, ( R (rNameT, flowT, tribsT) :: tributaries))

    let tryInsert name trib river : River = 
        match contains name river with 
            | true -> insert trib river
            | false -> R ("None", 0, [])


    let durationOf(appliance : Appliance, usageList : Usage list) : int =
        
        let rec durationOfRec(appliance, usageHead, usageList, time) =
            match usageHead, usageList with 
                | ((machine, minutes), []) when machine = appliance -> time + minutes
                | (_, []) -> time
                | ((machine, minutes), head::tail) when machine = appliance -> durationOfRec(appliance, head, tail, time + minutes)
                | (_, head::tail) -> durationOfRec(appliance, head, tail, time)

        durationOfRec(appliance, usageList.Head, usageList.Tail, 0)


    let isWellFormed(usageList : Usage list) : bool = 
        let mutable result = true
        for usage in usageList do
            match usage with 
                | (appliance, time) when durationOf(appliance, usageList) > 24 -> result <- false
                | _ -> () 

        result && inv(usageList)


    let rec delete(appliance : Appliance, usageList : Usage list) : Usage list =
        match usageList with 
            | ([]) -> []
            | ((machine, number)::tail) when appliance = machine -> delete(appliance, tail)
            | ((machine, number)::tail) -> (machine, number)::delete(appliance, tail)


    let isDefined(usageList : Usage list, trf : Tariff) : bool =
        let mutable appearance  = true
        for usage in usageList do
            match usage with 
                | (item, price) when trf.TryFind(item) = None -> appearance <- false
                | _ -> ()
        appearance
 
    let rec priceOf(usageList : Usage list, trf : Tariff) :  Price =
        match usageList with
            | [] -> 0
            | (item, price)::tail when trf.TryFind(item) = None -> failwith("priceOf: " + item + " is not in the list")
            | (item, price)::tail -> price + priceOf(tail, trf)

    [<EntryPoint>]
    let main argv =

        let ad4 = ("lavadora", 13) : Usage
        let ad5 = ("cafeton", 1) : Usage
        let ad6 = ("mierda", 2) : Usage
        let atsMal = [ad4; ad5; ad4; ad6; ad5] : Usage list

        printfn "inv method -> %b" (inv(ats))
        printfn "Duration of dishwasher in list -> %i" (durationOf("dishwasher", ats))
        printfn "Duration of washing machine in list -> %i" (durationOf("washing machine", ats))
        printfn "Duration of coffee machine in list -> %i" (durationOf("coffee machine", ats))
        printfn "Usage list well formed -> %b" (isWellFormed(ats))
        printfn "Usage list not well formed -> %b" (isWellFormed(atsMal))
        printfn "Delete coffee machine from ats -> %A" (delete("coffee machine", ats))
        // printfn "La puta mierda esta de G2 -> %A" (g2 2 3 4 (5 6) )
        printfn "Contains -> %b" (contains "R1" riv)
        printfn "Contains -> %b" (contains "R1" riv)
        printfn "Contains -> %b" (contains "R1" riv)
        printfn "Get flow -> %i" (totalFlow riv)
        printfn "Main Source -> %A" (mainSource riv)
        

        0;;