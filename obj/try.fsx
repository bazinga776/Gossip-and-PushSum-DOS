#time "on"
#r "nuget: Akka.FSharp"
#r "nuget: Akka.TestKit"
open System
open Akka.Actor
open Akka.Configuration
open Akka.FSharp
open System.Collections.Generic

type ProjTwo =
    | Initialize of IActorRef []
    | Algorithm of String
    | ReportMsgRecvd of String
    | TimeCount of int
    | NodeCount of int

let rnd = System.Random(1)

//logic for perfectSquare
let getPerfectSquareNum (x: float) =
    let mutable y = ceil (sqrt (x))
    printfn "y^2: %f"

type ActorCount() =
    inherit Actor()
    let mutable msgReceived = 0
    let mutable startTime = 0
    let mutable numOfNodes = 0

    override x.OnReceive(rmsg) =
        match rmsg :?> ProjTwo with
        | ReportMsgRecvd message ->
            let endTime =
                System.DateTime.Now.TimeOfDay.Milliseconds

            msgReceived <- msgReceived + 1
            if msgReceived = numOfNodes then
                printfn "Time for convergence: %i ms" (endTime - startTime)
                Environment.Exit(0)

        | TimeCount strtTime -> startTime <- strtTime

        | NodeCount numofnodes -> numOfNodes <- numofnodes

        | _ -> ()

type Node(actorCount: IActorRef, neighborlist: int [], nodestoping: int) =
    inherit Actor()
    let mutable numMsgHeard = 0
    let mutable nghbrs: IActorRef [] = [||]

    override x.OnReceive(num) =

        match num :?> ProjTwo with
        | Initialize aref -> nghbrs <- aref

        | Algorithm msg ->
            numMsgHeard <- numMsgHeard + 1
            if (numMsgHeard = 10) then
                actorCount <! ReportMsgRecvd(msg)

            else
                //to get randomly select next node, we use Random().Next(minValue, maxValue)
                let index = System.Random().Next(0, nghbrs.Length)
                nghbrs.[index] <! Algorithm(msg)

        | _ -> ()


//get input args
let mutable numNodes =
    int (string (fsi.CommandLineArgs.GetValue 1))

let topology = string (fsi.CommandLineArgs.GetValue 2)
let protocol = string (fsi.CommandLineArgs.GetValue 3)
let system = ActorSystem.Create("System")

let actorCount =
    system.ActorOf(Props.Create(typeof<ActorCount>), "actorCount")

match topology with
| "full" ->
    let mutable neighborlist: int list = []
    let buildTopology = new Dictionary<int, IActorRef>()
    for i in [ 0 .. numNodes + 1 ] do
        neighborlist <- i :: neighborlist //:: operator is used to append to the current list; mutable
    for i in [ 0 .. numNodes + 1 ] do
        buildTopology.Add(i, system.ActorOf(Props.Create(typeof<Node>, actorCount, i, neighborlist, 10), "projTwo"))

    //buildtopology called on the index of current node, its list of neighbors, total nodes to ping

    let boss = System.Random().Next(0, numNodes)
    if protocol = "gossip" then
        actorCount <! NodeCount(numNodes)
        actorCount
        <! TimeCount(System.DateTime.Now.TimeOfDay.Milliseconds)
        printfn "Result for Protocol Gossip"
        buildTopology.[boss]
        <! Algorithm("Gossip Protocol")

| "line" -> //actors placed linearly; actor at position i has neighbors at i+1 and i-1; max neighbrs possible 2, min 1
    let mutable neighborlist: int list = []
    let buildTopology = new Dictionary<int, IActorRef>()

    for i in [ 0 .. numNodes + 1 ] do
        if i = 1 then
            neighborlist <- i + 1 :: neighborlist
        elif i = numNodes then
            neighborlist <- i - 1 :: neighborlist
        else
            neighborlist <- i - 1 :: neighborlist
            neighborlist <- i + 1 :: neighborlist

        buildTopology.Add(i, system.ActorOf(Props.Create(typeof<Node>, actorCount, i, neighborlist, 10), "projTwo"))
        let boss = Random().Next(0, numNodes) //gossip protocol implementation on this
        if protocol = "gossip" then
            actorCount <! NodeCount(numNodes) //ask why it is used
            actorCount
            <! TimeCount(DateTime.Now.TimeOfDay.Milliseconds)
            printfn "Result for Protocol Gossip"
            buildTopology.[boss]
            <! Algorithm("Gossip Protocol")

| "2D" ->
    let mutable neighborlist: int list = []
    let buildTopology = new Dictionary<int, IActorRef>()
    let mutable squaredNode = getPerfectSquareNum (float numNodes)
    let mutable nodessqrt = sqrt (float numNodes)
    let mutable intsqrt = int (nodessqrt) //int conversion of nodessqrt
    // let buildTopology = Array2D.zeroCreate<int> intsqrt intsqrt //created 2D array with size of nodessqrt

    for i in [ 0 .. numNodes + 1 ] do
        if i = 1 then
            neighborlist <- i + 1 :: neighborlist
            neighborlist <- i + intsqrt :: neighborlist
        elif i = intsqrt then
            neighborlist <- i - 1 :: neighborlist
            neighborlist <- i + intsqrt :: neighborlist

        elif i = numNodes - intsqrt + 1 then
            neighborlist <- i + 1 :: neighborlist
            neighborlist <- i - intsqrt :: neighborlist

        elif i = numNodes then
            neighborlist <- i - 1 :: neighborlist
            neighborlist <- i - intsqrt :: neighborlist

        elif i < intsqrt then
            neighborlist <- i - 1 :: neighborlist
            neighborlist <- i + 1 :: neighborlist
            neighborlist <- i + intsqrt :: neighborlist

        elif (i > numNodes - intsqrt + 1 && i < numNodes) then
            neighborlist <- i - 1 :: neighborlist
            neighborlist <- i + 1 :: neighborlist
            neighborlist <- i - intsqrt :: neighborlist

        if ((i - 1 % intsqrt) = 0) then
            neighborlist <- i + 1 :: neighborlist
            neighborlist <- i - intsqrt :: neighborlist
            neighborlist <- i + intsqrt :: neighborlist

        if ((i % intsqrt) = 0) then
            neighborlist <- i - 1 :: neighborlist
            neighborlist <- i - intsqrt :: neighborlist
            neighborlist <- i + intsqrt :: neighborlist

        else
            neighborlist <- i - 1 :: neighborlist
            neighborlist <- i + 1 :: neighborlist
            neighborlist <- i - intsqrt :: neighborlist
            neighborlist <- i + intsqrt :: neighborlist

        buildTopology.Add(i, system.ActorOf(Props.Create(typeof<Node>, actorCount, i, neighborlist, 10), "projTwo"))

        let boss = Random().Next(0, numNodes) //gossip protocol implementation on this
        if protocol = "gossip" then
            actorCount <! NodeCount(numNodes) //ask why it is used
            actorCount
            <! TimeCount(DateTime.Now.TimeOfDay.Milliseconds)
            printfn "Result for Protocol Gossip"
            buildTopology.[boss]
            <! Algorithm("Gossip Protocol")

| "Imp2D" ->
    let mutable neighborlist: int list = []
    let buildTopology = new Dictionary<int, IActorRef>()
    let mutable squaredNode = getPerfectSquareNum (float numNodes)
    let mutable nodessqrt = sqrt (float numNodes)
    let mutable intsqrt = int (nodessqrt) //int conversion of nodessqrt
    for i in [ 0 .. numNodes + 1 ] do
        if i = 1 then
            neighborlist <- i + 1 :: neighborlist
            neighborlist <- i + intsqrt :: neighborlist
        elif i = intsqrt then
            neighborlist <- i - 1 :: neighborlist
            neighborlist <- i + intsqrt :: neighborlist

        elif i = numNodes - intsqrt + 1 then
            neighborlist <- i + 1 :: neighborlist
            neighborlist <- i - intsqrt :: neighborlist

        elif i = numNodes then
            neighborlist <- i - 1 :: neighborlist
            neighborlist <- i - intsqrt :: neighborlist

        elif i < intsqrt then
            neighborlist <- i - 1 :: neighborlist
            neighborlist <- i + 1 :: neighborlist
            neighborlist <- i + intsqrt :: neighborlist

        elif (i > numNodes - intsqrt + 1 && i < numNodes) then
            neighborlist <- i - 1 :: neighborlist
            neighborlist <- i + 1 :: neighborlist
            neighborlist <- i - intsqrt :: neighborlist

        if ((i - 1 % intsqrt) = 0) then
            neighborlist <- i + 1 :: neighborlist
            neighborlist <- i - intsqrt :: neighborlist
            neighborlist <- i + intsqrt :: neighborlist

        if ((i % intsqrt) = 0) then
            neighborlist <- i + 1 :: neighborlist
            neighborlist <- i - intsqrt :: neighborlist
            neighborlist <- i + intsqrt :: neighborlist

        else
            neighborlist <- i - 1 :: neighborlist
            neighborlist <- i + 1 :: neighborlist
            neighborlist <- i - intsqrt :: neighborlist
            neighborlist <- i + intsqrt :: neighborlist

        buildTopology.Add(i, system.ActorOf(Props.Create(typeof<Node>, actorCount, i, neighborlist, 10), "projTwo"))

        let boss = Random().Next(0, numNodes) //gossip protocol implementation on this
        if protocol = "gossip" then
            actorCount <! NodeCount(numNodes) //ask why it is used
            actorCount
            <! TimeCount(DateTime.Now.TimeOfDay.Milliseconds)
            printfn "Result for Protocol Gossip"
            buildTopology.[boss]
            <! Algorithm("Gossip Protocol")



































































| _ -> ()

System.Console.ReadLine() |> ignore
