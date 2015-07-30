module Events

open System
open System.Threading;
open AsyncUtils
open EventArgs

type SpikeEventArgs(spike) =
    inherit EventArgs()
    member this.Spike = spike

type TaskEventArgs(task) =
    inherit EventArgs()
    member this.Task = task

type BeliefEventArgs(belief) =
    inherit EventArgs()
    member this.Belief = belief

module Events =
    let syncContext = SynchronizationContext.CaptureCurrent()
    let SpikeEvent = new Event<SpikeEventArgs>()
    let TaskEvent = new Event<TaskEventArgs>()
    let BeliefEvent = new Event<BeliefEventArgs>()

    let raiseSpikeEvent(s) = syncContext.RaiseEvent SpikeEvent (SpikeEventArgs(s))
    let raiseTaskEvent(s) = syncContext.RaiseEvent TaskEvent (TaskEventArgs(s))
    let raiseBeliefEvent(s) = syncContext.RaiseEvent BeliefEvent (BeliefEventArgs(s))