namespace OldExamsWeek13

module Exam = 

    type Part = string
    type Task = string
    type Cost = int (* can be assumed to be positive *)
    type Duration = int (* can be assumed to be positive *)
    type PartReg = Map<Part, Cost>
    type TaskReg = Map<Task, Duration*Cost>

    (* Part and task registers for balance bikes *)
    let preg1 = Map.ofList [("wheel",50); ("saddle",10); ("handlebars",75); ("frame",100); ("screw bolt",5); ("nut",3)]
    let treg1 = Map.ofList [("addWheels",(10,2)); ("addSaddle",(5,2)); ("addHandlebars",(6,1))]


    type WorkStation = Task * (Part*int) list
    type AssemblyLine = WorkStation list

    let ws1 = ("addWheels",[("wheel",2);("frame",1);("screw bolt",2);("nut",2)])
    let ws2 = ("addSaddle",[("saddle",1);("screw bolt",1);("nut",1)])
    let ws3 = ("addHandlebars",[("handlebars",1);("screw bolt",1);("nut",1)])
    let al1 = [ws1; ws2; ws3]

    // 2.1
    let rec checkPregAndCost (preg : PartReg) wsList : bool = 
        match wsList with 
            | [] -> true
            | (part, cost)::tail when preg.TryFind(part) <> None && cost > 0 -> checkPregAndCost preg tail 
            | _ -> false

    let wellDefWS (preg : PartReg) (treg : TaskReg) (ws : WorkStation) : bool =
        let (task, wsList) = ws

        treg.TryFind(task) <> None && checkPregAndCost preg wsList 


    //let wellDefPL preg pl = List.forall (fun (p,n) -> n>0 && Map.containsKey p preg) pl 

    // 2.2
    let wellDefAL (preg : PartReg) (treg : TaskReg) (al : AssemblyLine) : bool =
        List.forall (fun (element) -> wellDefWS preg treg element) al
        //List.forall (wellDefWS preg treg) al

    // 2.3
    let longestDuration (al : AssemblyLine) (treg : TaskReg) : int =

        let rec getDuration (al : AssemblyLine) (treg : TaskReg) (dur : Duration) : Duration = 
            match al with 
            | [] -> dur
            | (task,_)::tail ->
                let (d, _) = Map.find task treg
                getDuration tail treg (max d dur)

        getDuration al treg 0

    // 2.4
    let rec partCostAux (wsList : (Part*int) list) (preg : PartReg) = 
        match wsList with 
            | [] -> 0
            | ((name, amount)::tail) ->
                let cost = Map.find name preg
                amount * cost + partCostAux tail preg

    let rec partCostAL (preg : PartReg) (al : AssemblyLine) : Cost =
        match al with 
            | [] -> 0
            | (_, partIntList)::tail -> partCostAux partIntList preg + partCostAL preg tail 

    
    // 2.5
    let rec prodDurCostAux (treg : TaskReg) (task : Task) = 
        let result = Map.find task treg 
        match result with
            | (duration, cost) -> duration, cost

    let rec prodDurCost (treg : TaskReg) (al : AssemblyLine) : Duration*Cost = 
        match al with 
            | [] -> 0,0
            | (task, _)::tail -> 
                let duration, cost = prodDurCostAux treg task
                let dur2, cost2 = prodDurCost treg tail
                duration + dur2, cost + cost2

    // 2.6
    type Stock = Map<Part, int>

    let rec toStockPL stk = function
                        | []    -> stk
                        | (p,n)::pl -> match Map.tryFind p stk with 
                                       | None   -> toStockPL (Map.add p n stk) pl   
                                       | Some k -> toStockPL (Map.add p (k+n) stk) pl 

    let rec toStockAL stk = function 
                       | []         -> stk
                       | (_,ps)::al -> toStockAL (toStockPL stk ps) al

    let toStock al = toStockAL Map.empty al


    // 3 May 2016
    type Container =
        | Tank of float * float * float // (length, width, height)
        | Ball of float // radius
        | Cylinder of float * float // (radius, height)

    // 3.1
    let tank1 = Tank(10.0, 10.0, 10.0)
    let ball1 = Ball 10.0
    let cylinder1 = Cylinder(10.0, 10.0)

    // 3.2
    let isWF container =
        match container with 
            | Tank(n1, n2, n3) when n1 > 0.0 && n2 > 0.0 && n3 > 0.0 -> true
            | Ball radius when radius > 0.0 -> true 
            | Cylinder(radius, height) when radius > 0.0 && height > 0.0 -> true 
            | _ -> false

    // 3.3
    let volume c =
        match c with
            | Tank(n1, n2, n3) -> n1*n2*n3
            | Ball radius -> (4.0/3.0)*(System.Math.PI)*(radius ** 3.0)
            | Cylinder (radius, height) -> (System.Math.PI)*(radius ** 2.0)*height // 3.4

    type Name = string
    type Contents = string
    type Storage = Map<Name, Contents*Container>

    // 3.5
    let storage1 = Map.ofList [("tank1",("oil", tank1)); ("ball1",("water", ball1))]

    // 3.6
    let find (n : Name) (stg : Storage) : Contents*float =
        let (contents, container) = Map.find n stg 
        contents, (volume container)

    // 4 May 2016
    type T<'a> = L | N of T<'a> * 'a * T<'a>
    let t = N(N(L, 1, N(N(L, 2, L), 1, L)), 3, L)

    // 4.1 T<int>
    let tList1 = N(N(L, [true; true], N(N(L, [false; false], L), [true; true], L)), [true; true; true], L)
    let tList2 = L
    let tList3 = N(L, [true], L)

    // 4.2 fail

    // 4.3
    let rec count a t =
        match t with 
            | L -> 0
            | N(t1, v, t2) when a = v -> 1 + count a t1 + count a t2
            | N(t1, v, t2) -> count a t1 + count a t2


    // 4.4
    let rec replace a b t =
        match t with 
            | L -> L
            | N(t1, v, t2) when a = v -> N(replace a b t1, b, replace a b t2)
            | N(t1, v, t2) -> N(replace a v t1, b, replace a v t2)

     
    [<EntryPoint>]
    let main argv =

        printfn "Part cost AL -> %i" (partCostAL preg1 al1)
        printfn "Product duration cost -> %A" (prodDurCost treg1 al1)
        printfn "To stock AL1 -> %A" (toStock al1)
        printfn "Find contents, float -> %A" (find "tank1" storage1)

        0;;