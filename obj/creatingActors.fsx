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
    | Input of (int64*string*string)

// Printer Actor - To print the output
let printerActor (mailbox:Actor<_>) = 
    let rec loop () = actor {
        printfn "printeractor"
        let! (index:int64) = mailbox.Receive()
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
        | BuildTopology(top, workerActorsPool) -> 
            
            printfn "build topology is called from worker actor"
            boss <! Done("Completed")
        | _ -> ()

        return! loop()
    }
    loop()
             
// Boss - Takes input from command line and spawns the actor pool. 
let BossActor (mailbox:Actor<_>) =
    
    let totalactors = 8L
    let workerActorsPool = 
            [1L .. totalactors]
            |> List.map(fun id -> spawn system (sprintf "Local_%d" id) WorkerActor)

    let workerenum = [|for lp in workerActorsPool -> lp|]
    let workerSystem = system.ActorOf(Props.Empty.WithRouter(Akka.Routing.RoundRobinGroup(workerenum)))
    let mutable completed = 0L

    let rec loop () = actor {
        let! message = mailbox.Receive()
        match message with 
        | Input(nodeNum, top, algo) -> 
            
            workerSystem <! BuildTopology(top, workerActorsPool)
            
        | Done(complete) ->
            completed <- completed + 1L
            printfn "bossactor done"
            if completed = completed then
                mailbox.Context.System.Terminate() |> ignore
        | _ -> ()
       
        return! loop()
    }
    loop()

let boss = spawn system "boss" BossActor
// Input from Command Line
let numNodes = fsi.CommandLineArgs.[1] |> int64
let topology = fsi.CommandLineArgs.[2] |> string
let algorithm = fsi.CommandLineArgs.[3] |> string

boss <! Input(numNodes, topology, algorithm)
// Wait until all the actors has finished processing
system.WhenTerminated.Wait() 