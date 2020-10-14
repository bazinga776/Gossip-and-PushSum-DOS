#time "on"
#r "nuget: Akka.FSharp"
#r "nuget: Akka.TestKit"
open System
open Akka.Actor
open Akka.Configuration
open Akka.FSharp
open System.Collections.Generic

type ProjTwo =
    |Initailize of IActorRef[]
    |Algorithm of String
    |ReportMsgRecvd of String
    |TimeCount of int
    |NodeCount of int
let rnd  = System.Random(1)

type ActorCount() =
    inherit Actor()
    let mutable msgReceived = 0
    let mutable startTime = 0
    let mutable numOfNodes =0

    override x.OnReceive(rmsg) = 
        match rmsg :?>ProjTwo with 
        | ReportMsgRecvd message ->
            let endTime = System.DateTime.Now.TimeOfDay.Milliseconds
            msgReceived <- msgReceived + 1
            if msgReceived = numOfNodes then
                printfn "Time for convergence: %i ms" (endTime-startTime)
                Environment.Exit(0)  
           
        | TimeCount strtTime ->
            startTime <-strtTime

        | NodeCount numofnodes ->
            numOfNodes <-numofnodes

        | _->()
 
type Node(actorCount: IActorRef, numResend: int, nodeNum: int)=
    inherit Actor()
    let mutable numMsgHeard = 0 
    let mutable  nghbrs:IActorRef[]=[||]
 
    override x.OnReceive(num)=
         
         match num :?>ProjTwo with 
         | Initailize aref->
                nghbrs<-aref

         | Algorithm msg ->
                numMsgHeard<- numMsgHeard+1
                
                if(numMsgHeard=10) then 
                      actorCount <! ReportMsgRecvd(msg)
                
                if(numMsgHeard <=100) then
                        //to get randomly select next node, we use Random().Next(minValue, maxValue) 
                        let index= System.Random().Next(0,nghbrs.Length)
                        nghbrs.[index] <! Algorithm(msg)

         | _-> ()


//get input args
let mutable numNodes = int(string (fsi.CommandLineArgs.GetValue 1))
let topology = string (fsi.CommandLineArgs.GetValue 2)
let algorithm = string (fsi.CommandLineArgs.GetValue 3)

let system = ActorSystem.Create("System")
let mutable actualNumOfNodes=float(numNodes)


numNodes = if topology="2D" || topology="imp2D" then 
                 int(floor((actualNumOfNodes ** 0.5) ** 2.0))
             else
                 numNodes
         
let actorCount=system.ActorOf(Props.Create(typeof<ActorCount>),"actorCount")

match topology  with 
      | "full"->
          let buildTopology = Array.zeroCreate (numNodes+1)
          for i in [0..numNodes] do
              buildTopology.[i]<-system.ActorOf(Props.Create(typeof<Node>,actorCount,10,i+1),"projTwo"+string(i))
          for i in [0..numNodes] do
              buildTopology.[i]<!Initailize(buildTopology)
              
          let boss = System.Random().Next(0,numNodes)
          if algorithm="gossip" then
            actorCount<!NodeCount(numNodes)
            actorCount<!TimeCount(System.DateTime.Now.TimeOfDay.Milliseconds)
            printfn "Result for Protocol Gossip"
            buildTopology.[boss]<!Algorithm("Gossip Algorithm for Full Topology")

      |"line"->
          let buildTopology= Array.zeroCreate (numNodes+1)
          for i in [0..numNodes] do
              buildTopology.[i]<-system.ActorOf(Props.Create(typeof<Node>,actorCount,10,i+1),"ProjTwo"+string(i))
          for i in [0..numNodes] do
              let nghbrArray=[|buildTopology.[((i-1+numNodes)%(numNodes+1))];buildTopology.[((i+1+numNodes)%(numNodes+1))]|]
              buildTopology.[i]<!Initailize(nghbrArray)
          let boss = System.Random().Next(0,numNodes)
          if algorithm="gossip" then
            actorCount<!NodeCount(numNodes)
            actorCount<!TimeCount(System.DateTime.Now.TimeOfDay.Milliseconds)
            printfn "Result for Protocol Gossip"
            buildTopology.[boss]<!Algorithm("Gossip Algorithm for Line Topology")


      |"2D"->
           let gridSize=int(ceil(sqrt actualNumOfNodes))
           let gridCount=gridSize*gridSize
           let buildTopology= Array.zeroCreate (gridCount)
           for i in [0..(gridSize*gridSize-1)] do
              buildTopology.[i]<-system.ActorOf(Props.Create(typeof<Node>,actorCount,10,i+1),"ProjTwo"+string(i))
           
           for i in [0..gridSize-1] do
               for j in [0..gridSize-1] do
                    let mutable nghbrs:IActorRef[]=[||]
                    if j+1<gridSize then
                        nghbrs<-(Array.append nghbrs [|buildTopology.[i*gridSize+j+1]|])
                    if j-1>=0 then
                        nghbrs<-Array.append nghbrs [|buildTopology.[i*gridSize+j-1]|]
                    if i-1>=0 then
                        nghbrs<-Array.append nghbrs [|buildTopology.[(i-1)*gridSize+j]|]
                    if i+1<gridSize then
                        nghbrs<-(Array.append nghbrs [|buildTopology.[(i+1)*gridSize+j]|])
                    buildTopology.[i*gridSize+j]<!Initailize(nghbrs)

       
               
           let boss = System.Random().Next(0,gridCount-1)  
           if algorithm="gossip" then
            actorCount<!NodeCount(gridCount-1)
            actorCount<!TimeCount(System.DateTime.Now.TimeOfDay.Milliseconds)
            printfn "Result for Protocol Gossip"
            buildTopology.[boss]<!Algorithm("Gossip Algorithm for 2D Topology")


      | _-> ()
      
System.Console.ReadLine()|>ignore

       