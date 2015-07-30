module AsyncUtils

open System.Threading

type SynchronizationContext with 
    /// A standard helper extension method to raise an event on the GUI thread
    member syncContext.RaiseEvent (event: Event<_>) args = 
        syncContext.Post((fun _ -> event.Trigger args),state=null)

    /// A standard helper extension method to capture the current synchronization context.
    /// If none is present, use a context that executes work in the thread pool.
    static member CaptureCurrent () = 
        match SynchronizationContext.Current with 
        | null -> new SynchronizationContext()
        | ctxt -> ctxt