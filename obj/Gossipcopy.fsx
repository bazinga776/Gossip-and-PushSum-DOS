#time "on"
#r "nuget: Akka.FSharp"
#r "nuget: Akka.TestKit"
open System
open Akka.Actor
open Akka.FSharp

let system = ActorSystem.Create("Gossip", Configuration.defaultConfig())
type Information = 
    | BuildTopology of (string*IActorRef list)
    | Done of (string)
    | Input of (int*string*string)

// Printer Actor - To print the output
let printerActor (mailbox:Actor<_>) = 
    let rec loop () = actor {
        printfn "printeractor"
        let! (index:int) = mailbox.Receive()
        printfn "printeractor1"
        printfn "%d" index      
        return! loop()
    }
    loop()

let printerRef = spawn system "Printer" printerActor

// Worker Actors - Takes input from Boss and do the processing and returns the completed message.
let WorkerActor (mailbox:Actor<_>) =
    let rec loop () = actor {
        let! message = mailbox.Receive()
        let boss = mailbox.Sender()
        match message with
        | BuildTopology(top, workerActorsPool) ->  //worker actors pool defined later hence not called here
            
            printfn "build topology is called from worker actor"
            boss <! Done("Completed")
        | _ -> ()

        return! loop()
    }
    loop()
             
// Boss - Takes input from command line and spawns the actor pool. 
//let boss = spawn system "boss" BossActor

let BossActor (mailbox:Actor<_>) =
    match message with 
        | Input(actorsNum, top, algo) -> 
        let totalactors = actorsNum
        let workerActorsPool = 
                [1 .. totalactors]
                |> List.map(fun id -> spawn system (sprintf "Local_%d" id) WorkerActor)
        let workerenum = [|for lp in workerActorsPool -> lp|]
        let workerSystem = system.ActorOf(Props.Empty.WithRouter(Akka.Routing.RoundRobinGroup(workerenum)))
        let mutable completed = 0
        let rec loop () = actor { 
            workerSystem <! BuildTopology(top, workerActorsPool)  
                |Done(complete) ->
                    completed <- completed + 1
                    printfn "bossactor done"
                    if completed = completed then //what is the use of this? 
                        mailbox.Context.System.Terminate() 
                | _ -> ()

                return! loop()
        }
        loop()
    
//main function which accepts arguments from commandline
let main (numNodes: int, topology: string, algorithm: string) =
    if not (isValidInput (numNodes, topology, algorithm)) then 
        printfn "Error: Invalid Values for required arguments"

    else
        if topo = "2D" || topo = "imp2D" then
            numNodes = getNextPerfectSquare(numNodes)
    end

    let list = [1..numNodes]
    list |> List.map (fun v1 -> List.map (fun x -> let pid = start_node() updatePIDState(pid, x)) list) |> List.collect id

    //state maintaining logic of prcoesses and actors 
    //hashtable and lookup logic

    //build topology called here

    let BuildTopology(topology,numNodes) =
        let startTime f x = System.Diagnostics.Stopwatch.StartNew() |> (fun sw -> (f x, sw.Elapsed))
    0

    //start the algorithm here
    //logic for convergence 

   let BuildTopology(algorithm, numNodes, startTime) =
        match topology with 
        | "full" -> createFull(numNodes)
        | "2D" -> create2D(numNodes)
        | "line" -> createLine(numNodes)
        | "imp2D" -> createImp2D(numNodes)

    // create functions for all individual topologies

    let createFull(numNodes) =
    let create2D(numNodes) =
    let createLine(numNodes) =
    let createImp2D(numNodes) =
    let doFunc() =


//getPerfectSquareNum takes one int argument and returns int value
let doFunc() =
    //getPerfectSquareNum takes one int argument and returns int value
    let getPerfectSquareNum (x: int) : int = pown x 2 
    printfn "x^2 = %i" (getPerfectSquareNum(10))
 
doFunc()

//Code for reading command line inputs and passing to main function
    match fsi.CommandLineArgs with
    | [|_; numNodes; topology; algorithm|] -> 
        let nodesval = int numNodes
        let topoval = string topology
        let algoval = string algorithm
        main (nodesval, topoval, algoval)
    | _ -> printfn "Error: Invalid Arguments"
