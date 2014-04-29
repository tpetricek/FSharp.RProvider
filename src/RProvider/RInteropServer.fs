namespace RProviderServer

open Microsoft.FSharp.Core.CompilerServices
open Microsoft.Win32
open RDotNet
open RProvider
open RProvider.RInterop
open RProvider.Internal
open System

type RInteropServer() =
    inherit MarshalByRefObject()
    
    // Set the R 'R_CStackLimit' variable to -1 when initializing the R engine
    // (the engine is initialized lazily, so the initialization always happens
    // after the static constructor is called - by doing this in the static constructor
    // we make sure that this is *not* set in the normal execution)
    static do RInit.DisableStackChecking <- true
    
    let initResultValue = RInit.initResult.Force()
    let serverLock = "serverLock"
    let withLock f =
        lock serverLock f

    let mutable remoteSessions = Map.empty

    member x.RInitValue
        with get() =
            withLock <| fun () ->
            match initResultValue with
            | RInit.RInitError error -> Some error
            | _ -> None

    member private x.GetRemoteSession(config:SessionConfig) =
        let sessionKey = (config.hostName, config.port, config.blocking)
        if not (remoteSessions.ContainsKey sessionKey) then 
            remoteSessions <- remoteSessions.Add(sessionKey, RemoteSession.GetConnection(config))
        remoteSessions.[sessionKey]

    member x.GetPackages() =
         withLock <| fun () ->
            getPackages()

    member x.GetPackages(remoteSession) =
        withLock <| fun () ->
            x.GetRemoteSession(remoteSession).getPackages()
         
    member x.LoadPackage(package) =
        withLock <| fun () ->
            loadPackage package

    member x.LoadPackage(package, remoteSession) =
        withLock <| fun () ->
            x.GetRemoteSession(remoteSession).loadPackage package
        
    member x.GetBindings(package, remoteSession) =
        withLock <| fun () ->
            x.GetRemoteSession(remoteSession).getBindings package

    member x.GetBindings(package) =
        withLock <| fun () ->
            getBindings package
        
    member x.GetFunctionDescriptions(package:string, remoteSession) =
        withLock <| fun () ->
            x.GetRemoteSession(remoteSession).getFunctionDescriptions package
    
    member x.GetFunctionDescriptions(package:string) =
        withLock <| fun () ->
            getFunctionDescriptions package
        
    member x.GetPackageDescription(package, remoteSession) =
        withLock <| fun () ->
            x.GetRemoteSession(remoteSession).getPackageDescription package
    
    member x.GetPackageDescription(package) =
        withLock <| fun () ->
            getPackageDescription package
        
    member x.MakeSafeName(name) =
        withLock <| fun () ->
            makeSafeName name


