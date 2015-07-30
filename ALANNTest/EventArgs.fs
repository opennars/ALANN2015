module EventArgs

open System
open AsyncUtils
open Types

type SpikeEventArgs(spike) =
    inherit EventArgs()
    member this.Spike = spike

type TaskEventArgs(task) =
    inherit EventArgs()
    member this.Task = task

type BeliefEventArgs(belief) =
    inherit EventArgs()
    member this.Belief = belief