namespace Concept

open System
open System.Collections.Generic
open System.Collections.Concurrent
open Akka.Actor
open Akka.FSharp
open Types

type Message =
    | Vote of int
    | Task of Task

//type Concept() =    
//    let mutable voteCount = 0
//    let mutable lastElection = 0L
//    let mutable lastUpdate = 0L
//    let beliefs = List<Belief>()
//
//    let requiredVotes = 20
//
//    let discountVotes votes =
//        // Discount votes based on expontial decay over time
//        votes
//
//    let handleVote vote = 
//        // discount votes since time has passed
//        voteCount <- discountVotes voteCount
//
//        // add vote to voteCount
//        voteCount <- voteCount + vote
//
//        // Hold an instant election
//        if voteCount > requiredVotes then
//            // get reelected
//            // reset votes
//            voteCount <- 0
//            // vote on everything you represent/believe in
//            for belief in beliefs do
//                // send vote to belief representative
//
//            
//
//    let handleTask task = ()
//
//    let handleMessage (mailbox: Actor<'a>) msg =
//        match msg with
//        | Vote(vote) -> handleVote vote
//        | Task(task) -> handleTask task
//
