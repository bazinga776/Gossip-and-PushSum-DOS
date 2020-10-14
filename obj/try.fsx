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
    | Push of double * double
    | PushSumAnswer of double * double

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

        | PushSumAnswer (s, w) ->
            printfn "sum %f weight %f average %f" s w (s / w)
            printfn "Time of convergence endtime - startime"
            Environment.Exit 0

        | TimeCount strtTime -> startTime <- strtTime

        | NodeCount numofnodes -> numOfNodes <- numofnodes

        | _ -> ()

type Node(actorCount: IActorRef, totalnodes: int, neighborlist: IActorRef) =
    inherit Actor()
    let mutable numMsgHeard = 0
    let mutable nghbrs: IActorRef [] = [||]
    let mutable rounds = 0
    let mutable s = totalnodes |> double
    let mutable w = 1.0

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

        | Push (incomingS, incomingW) ->
            let mutable newS = s + incomingS
            let mutable newW = w + incomingW

            let difference = abs ((newS / newW) - (s / w))

            if (difference > pown 10.0 -10) then
                rounds <- 0
                s <- s / 2.0
                w <- w / 2.0

                let index = System.Random().Next(0, nghbrs.Length)
                nghbrs.[index] <! Push(s, w)

            elif (rounds >= 3) then
                actorCount <! PushSumAnswer(s, w)

            else
                let index = System.Random().Next(0, nghbrs.Length)
                s <- s / 2.0
                w <- w / 2.0
                nghbrs.[index] <! Push(s, w)
                rounds <- rounds + 1

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
    let mutable neighborlist: IActorRef list = [] //list that stores neighbors of current node i
    let buildTopology = Array.zeroCreate (numNodes + 1) //to store all actor nodes in one place

    for i in [ 0 .. numNodes + 1 ] do
        buildTopology.[i] <- system.ActorOf(Props.Create(typeof<Node>, actorCount, numNodes, neighborlist), "ProjTwo") //The actor function needs to be passed here; fix this

    for i in [ 0 .. numNodes + 1 ] do //list that stores the neighbors for the node i; here all actors are added to the list for every i
        neighborlist <- buildTopology.[i] :: neighborlist //:: operator is used to append to the current list; mutable

    //to do: call the node function with the list and limit 10

    let listlength = neighborlist.Length //since a random neighbor is selected to send rumor

    let boss = System.Random().Next(0, listlength)
    if protocol = "gossip" then
        actorCount <! NodeCount(numNodes)
        actorCount
        <! TimeCount(System.DateTime.Now.TimeOfDay.Milliseconds)
        printfn "Result for Protocol Gossip"
        buildTopology.[boss]
        <! Algorithm("Gossip Protocol")

    if protocol = "pushsum" then
        for i in [ 0 .. numNodes + 1 ] do //picking a random neighbor to send s/w pair
            let mutable s = i
            let mutable w = 1.0

            //for current i, set s and w, find a random neighbor and send your s/w pair information
            for i in [ 0 .. listlength + 1 ] do //picking the random neighbor from the list
                actorCount (numNodes)
                actorCount
                <! TimeCount(System.DateTime.Now.TimeOfDay.Milliseconds) //time started
                printfn "Result from Protocol Pushsum"
                buildTopology.[boss]
                <! PushSumAnswer(s / 2, w / 2)
//in the node function,


| "line" -> //actors placed linearly; actor at position i has neighbors at i+1 and i-1; max neighbrs possible 2, min 1
    let mutable neighborlist: IActorRef list = [] //list that stores neighbors of current node i
    let buildTopology = Array.zeroCreate (numNodes + 1) //to store all actor nodes in one place

    for i in [ 0 .. numNodes + 1 ] do
        buildTopology.[i] <- system.ActorOf(Props.Create(typeof<Node>, actorCount, numNodes, neighborlist), "ProjTwo") //the actor function should be passed here; please check this

    for i in [ 0 .. numNodes + 1 ] do
        if i = 1 then
            neighborlist <- buildTopology.[i + 1] :: neighborlist
        elif i = numNodes then
            neighborlist <- buildTopology.[i - 1] :: neighborlist
        else
            neighborlist <- buildTopology.[i - 1] :: neighborlist
            neighborlist <- buildTopology.[i + 1] :: neighborlist

        let boss = Random().Next(0, numNodes) //gossip protocol implementation on this
        if protocol = "gossip" then
            actorCount <! NodeCount(numNodes) //ask why it is used
            actorCount
            <! TimeCount(DateTime.Now.TimeOfDay.Milliseconds)
            printfn "Result for Protocol Gossip"
            buildTopology.[boss]
            <! Algorithm("Gossip Protocol")


| "2D" ->
    let mutable neighborlist: IActorRef list = [] //list that stores neighbors of current node i
    let buildTopology = Array.zeroCreate (numNodes + 1) //to store all actor nodes in one place
    for i in [ 0 .. numNodes + 1 ] do
        buildTopology.[i] <- (spawn system (string i) Node)

    let mutable squaredNode = getPerfectSquareNum (float numNodes)
    let mutable nodessqrt = sqrt (float numNodes)
    let mutable intsqrt = int (nodessqrt) //int conversion of nodessqrt
    // let buildTopology = Array2D.zeroCreate<int> intsqrt intsqrt //created 2D array with size of nodessqrt

    for i in [ 0 .. numNodes + 1 ] do
        if i = 1 then
            neighborlist <- buildTopology.[i + 1] :: neighborlist
            neighborlist <- buildTopology.[i + intsqrt] :: neighborlist
        elif i = intsqrt then
            neighborlist <- buildTopology.[i - 1] :: neighborlist
            neighborlist <- buildTopology.[i + intsqrt] :: neighborlist

        elif i = numNodes - intsqrt + 1 then
            neighborlist <- buildTopology.[i + 1] :: neighborlist
            neighborlist <- buildTopology.[i - intsqrt] :: neighborlist

        elif i = numNodes then
            neighborlist <- buildTopology.[i - 1] :: neighborlist
            neighborlist <- buildTopology.[i - intsqrt] :: neighborlist

        elif i < intsqrt then
            neighborlist <- buildTopology.[i - 1] :: neighborlist
            neighborlist <- buildTopology.[i + 1] :: neighborlist
            neighborlist <- buildTopology.[i + intsqrt] :: neighborlist

        elif (i > numNodes - intsqrt + 1 && i < numNodes) then
            neighborlist <- buildTopology.[i - 1] :: neighborlist
            neighborlist <- buildTopology.[i + 1] :: neighborlist
            neighborlist <- buildTopology.[i - intsqrt] :: neighborlist

        if ((i - 1 % intsqrt) = 0) then
            neighborlist <- buildTopology.[i + 1] :: neighborlist
            neighborlist <- buildTopology.[i - intsqrt] :: neighborlist
            neighborlist <- buildTopology.[i + intsqrt] :: neighborlist

        if ((i % intsqrt) = 0) then
            neighborlist <- buildTopology.[i - 1] :: neighborlist
            neighborlist <- buildTopology.[i - intsqrt] :: neighborlist
            neighborlist <- buildTopology.[i + intsqrt] :: neighborlist

        else
            neighborlist <- buildTopology.[i - 1] :: neighborlist
            neighborlist <- buildTopology.[i + 1] :: neighborlist
            neighborlist <- buildTopology.[i - intsqrt] :: neighborlist
            neighborlist <- buildTopology.[i + intsqrt] :: neighborlist

        let boss = Random().Next(0, numNodes) //gossip protocol implementation on this
        if protocol = "gossip" then
            actorCount <! NodeCount(numNodes)
            actorCount
            <! TimeCount(DateTime.Now.TimeOfDay.Milliseconds)
            printfn "Result for Protocol Gossip"
            buildTopology.[boss]
            <! Algorithm("Gossip Protocol")

| "Imp2D" ->
    let mutable neighborlist: IActorRef list = [] //list that stores neighbors of current node i
    let buildTopology = Array.zeroCreate (numNodes + 1) //to store all actor nodes in one place
    for i in [ 0 .. numNodes + 1 ] do
        buildTopology.[i] <- system.ActorOf
                                 (Props.Create(typeof<Node>, actorCount, numNodes, neighborlist), "ProjTwo" + string (i))
    let mutable squaredNode = getPerfectSquareNum (float numNodes)
    let mutable nodessqrt = sqrt (float numNodes)
    let mutable intsqrt = int (nodessqrt) //int conversion of nodessqrt
    for i in [ 0 .. numNodes + 1 ] do
        if i = 1 then
            neighborlist <- buildTopology.[i + 1] :: neighborlist
            neighborlist <- buildTopology.[i + intsqrt] :: neighborlist
        elif i = intsqrt then
            neighborlist <- buildTopology.[i - 1] :: neighborlist
            neighborlist <- buildTopology.[i + intsqrt] :: neighborlist

        elif i = numNodes - intsqrt + 1 then
            neighborlist <- buildTopology.[i + 1] :: neighborlist
            neighborlist <- buildTopology.[i - intsqrt] :: neighborlist

        elif i = numNodes then
            neighborlist <- buildTopology.[i - 1] :: neighborlist
            neighborlist <- buildTopology.[i - intsqrt] :: neighborlist

        elif i < intsqrt then
            neighborlist <- buildTopology.[i - 1] :: neighborlist
            neighborlist <- buildTopology.[i + 1] :: neighborlist
            neighborlist <- buildTopology.[i + intsqrt] :: neighborlist

        elif (i > numNodes - intsqrt + 1 && i < numNodes) then
            neighborlist <- buildTopology.[i - 1] :: neighborlist
            neighborlist <- buildTopology.[i + 1] :: neighborlist
            neighborlist <- buildTopology.[i - intsqrt] :: neighborlist

        if ((i - 1 % intsqrt) = 0) then
            neighborlist <- buildTopology.[i + 1] :: neighborlist
            neighborlist <- buildTopology.[i - intsqrt] :: neighborlist
            neighborlist <- buildTopology.[i + intsqrt] :: neighborlist

        if ((i % intsqrt) = 0) then
            neighborlist <- buildTopology.[i + 1] :: neighborlist
            neighborlist <- buildTopology.[i - intsqrt] :: neighborlist
            neighborlist <- buildTopology.[i + intsqrt] :: neighborlist

        else
            neighborlist <- buildTopology.[i - 1] :: neighborlist
            neighborlist <- buildTopology.[i + 1] :: neighborlist
            neighborlist <- buildTopology.[i - intsqrt] :: neighborlist
            neighborlist <- buildTopology.[i + intsqrt] :: neighborlist

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
