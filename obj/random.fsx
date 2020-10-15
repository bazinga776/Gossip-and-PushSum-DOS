#time "on"
#r "nuget: Akka.FSharp"
#r "nuget: Akka.TestKit"
open System
open Akka.Actor
open Akka.FSharp

type ProjTwo =
    | Initialize of IActorRef []
    | AlgoGossip of String
    | Rumor of String
    | AlgoPushSum of Double
    | PushSumCalc of Double * Double * Double
    | PushSumConv of Double * Double
    | TimeCount of int
    | NodeCount of int

type ActorCount() =
    inherit Actor()
    let mutable totalmsgs = 0
    let mutable startTime = 0
    let mutable numOfNodes = 0

    override x.OnReceive(rmsg) =
        match rmsg :?> ProjTwo with
        | Rumor message ->
            let endTime =
                System.DateTime.Now.TimeOfDay.Milliseconds

            totalmsgs <- totalmsgs + 1
            if totalmsgs = numOfNodes then
                let convergence = (endTime - startTime)
                printfn "Covergence Time: %i " convergence
                Environment.Exit(0)

        | PushSumConv (sumValue, weightValue) ->
            let endTime =
                System.DateTime.Now.TimeOfDay.Milliseconds
            //printfn "Sum = %f Weight= %f Average=%f" sumValue weightValue (sumValue / weightValue)
            let convergence = (endTime - startTime)
            printfn "Convergence Time: %i" convergence
            Environment.Exit(0)

        | _ -> ()

type Node(actorCount: IActorRef, transmitmsgs: int, nodeNum: int) =
    inherit Actor()
    let mutable totalmsgs = 0
    let mutable nghbrs: IActorRef [] = [||]

    let mutable sCurr = nodeNum |> float
    let mutable wCurr = 1.0
    let mutable rounds = 1
    let roundlimit = 3
    let transmitlimit = 10

    override x.OnReceive(num) =

        match num :?> ProjTwo with
        | Initialize aref -> nghbrs <- aref

        | AlgoGossip msg ->
            totalmsgs <- totalmsgs + 1

            if (totalmsgs = transmitlimit) then actorCount <! Rumor(msg)
            if (totalmsgs <= (transmitlimit * nghbrs.Length)) then
                let index = System.Random().Next(0, nghbrs.Length)
                nghbrs.[index] <! AlgoGossip(msg)

        | AlgoPushSum limit ->
            let index = System.Random().Next(0, nghbrs.Length)
            sCurr <- sCurr / 2.0
            wCurr <- wCurr / 2.0
            nghbrs.[index] <! PushSumCalc(sCurr, wCurr, limit)

        | PushSumCalc (incomingS: float, incomingW: float, limit) ->
            let newsum = sCurr + incomingS
            let newweight = wCurr + incomingW

            let difference =
                sCurr / wCurr - newsum / newweight |> abs

            if (rounds >= roundlimit) then
                actorCount <! PushSumConv(sCurr, wCurr)

            else if (difference > limit) then
                rounds <- 0
                sCurr <- sCurr + incomingS
                wCurr <- wCurr + incomingW
                sCurr <- sCurr / 2.0
                wCurr <- wCurr / 2.0
                let index = System.Random().Next(0, nghbrs.Length)
                nghbrs.[index] <! PushSumCalc(sCurr, wCurr, limit)

            else
                sCurr <- sCurr / 2.0
                wCurr <- wCurr / 2.0
                rounds <- rounds + 1
                let index = System.Random().Next(0, nghbrs.Length)
                nghbrs.[index] <! PushSumCalc(sCurr, wCurr, limit)
        | _ -> ()


//Receive arguments from the terminal
let mutable numNodes =
    int (string (fsi.CommandLineArgs.GetValue 1))

let topology = string (fsi.CommandLineArgs.GetValue 2)
let algorithm = string (fsi.CommandLineArgs.GetValue 3)

let system = ActorSystem.Create("System")
let mutable actualNumOfNodes = float (numNodes)

numNodes =
    if topology = "2D" || topology = "imp2D"
    then int (floor ((actualNumOfNodes ** 0.5) ** 2.0))
    else numNodes

let actorCount =
    system.ActorOf(Props.Create(typeof<ActorCount>), "actorCount")

match topology with
| "full" ->
    let buildTopology = Array.zeroCreate (numNodes + 1)
    for i in [ 0 .. numNodes ] do
        buildTopology.[i] <- system.ActorOf(Props.Create(typeof<Node>, actorCount, 100, i + 1), "projTwo" + string (i))
    for i in [ 0 .. numNodes ] do
        buildTopology.[i] <! Initialize(buildTopology)

    let boss = System.Random().Next(0, numNodes)
    if algorithm = "gossip" then
        actorCount <! NodeCount(numNodes)
        actorCount
        <! TimeCount(System.DateTime.Now.TimeOfDay.Milliseconds)
        printfn "Result for Protocol Gossip"
        buildTopology.[boss]
        <! AlgoGossip("Gossip Algorithm for Full Topology")
    else if algorithm = "pushsum" then
        actorCount
        <! TimeCount(System.DateTime.Now.TimeOfDay.Milliseconds)
        printfn "Push sum algorithm for Full"
        buildTopology.[boss] <! AlgoPushSum(10.0 ** -10.0)



| "line" ->
    let buildTopology = Array.zeroCreate (numNodes + 1)
    for i in [ 0 .. numNodes ] do
        buildTopology.[i] <- system.ActorOf(Props.Create(typeof<Node>, actorCount, 10, i + 1), "ProjTwo" + string (i))
    for i in [ 0 .. numNodes ] do
        let nghbrArray =
            [| buildTopology.[((i - 1 + numNodes) % (numNodes + 1))]
               buildTopology.[((i + 1 + numNodes) % (numNodes + 1))] |]

        buildTopology.[i] <! Initialize(nghbrArray)
    let boss = System.Random().Next(0, numNodes)
    if algorithm = "gossip" then
        actorCount <! NodeCount(numNodes)
        actorCount
        <! TimeCount(System.DateTime.Now.TimeOfDay.Milliseconds)
        printfn "Result for Protocol Gossip"
        buildTopology.[boss]
        <! AlgoGossip("Gossip Algorithm for Line Topology")
    else if algorithm = "pushsum" then
        actorCount
        <! TimeCount(System.DateTime.Now.TimeOfDay.Milliseconds)
        printfn "Push sum algorithm for Line"
        buildTopology.[boss] <! AlgoPushSum(10.0 ** -10.0)



| "2D" ->
    let gridSize = int (ceil (sqrt actualNumOfNodes))
    let gridCount = gridSize * gridSize
    let buildTopology = Array.zeroCreate (gridCount)
    for i in [ 0 .. (gridSize * gridSize - 1) ] do
        buildTopology.[i] <- system.ActorOf(Props.Create(typeof<Node>, actorCount, 10, i + 1), "ProjTwo" + string (i))

    for i in [ 0 .. gridSize - 1 ] do
        for j in [ 0 .. gridSize - 1 ] do
            let mutable nghbrs: IActorRef [] = [||]
            if j + 1 < gridSize
            then nghbrs <- (Array.append nghbrs [| buildTopology.[i * gridSize + j + 1] |])
            if j - 1 >= 0
            then nghbrs <- Array.append nghbrs [| buildTopology.[i * gridSize + j - 1] |]
            if i - 1 >= 0
            then nghbrs <- Array.append nghbrs [| buildTopology.[(i - 1) * gridSize + j] |]
            if i + 1 < gridSize
            then nghbrs <- (Array.append nghbrs [| buildTopology.[(i + 1) * gridSize + j] |])

            buildTopology.[i * gridSize + j]
            <! Initialize(nghbrs)


    let boss = System.Random().Next(0, gridCount - 1)
    if algorithm = "gossip" then
        actorCount <! NodeCount(gridCount - 1)
        actorCount
        <! TimeCount(System.DateTime.Now.TimeOfDay.Milliseconds)
        printfn "Result for Protocol Gossip"
        buildTopology.[boss]
        <! AlgoGossip("Gossip Algorithm for 2D Topology")

    else if algorithm = "pushsum" then
        actorCount
        <! TimeCount(System.DateTime.Now.TimeOfDay.Milliseconds)
        printfn "Push sum algorithm for 2D"
        buildTopology.[boss] <! AlgoPushSum(10.0 ** -10.0)



| _ -> ()

System.Console.ReadLine() |> ignore
