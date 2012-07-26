﻿namespace RProvider

open System
open System.Collections.Generic
open System.Reflection
open System.IO
open System.Diagnostics
open System.Threading
open Samples.FSharp.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open RDotNet
open RInteropInternal
open RInterop

[<TypeProvider>]
type public RProvider(cfg:TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    // Get the assembly and namespace used to house the provided types
    let asm = System.Reflection.Assembly.GetExecutingAssembly()
    let ns = "RProvider"

    // Expose all available packages as namespaces
    do
        for package in getPackages() do
            let pns = ns + "." + package
            let pty = ProvidedTypeDefinition(asm, pns, "R", Some(typeof<obj>))    

            pty.AddXmlDocDelayed <| fun () -> getPackageDescription package
            pty.AddMembersDelayed( fun () -> 
              [ loadPackage package
                let bindings = getBindings package

                // We get the function descriptions for R the first time they are needed
                let titles = lazy getFunctionDescriptions package

                for name, rval in Map.toSeq bindings do
                    let memberName = name.Replace("_","__").Replace(".", "_")
                    match rval with
                    | RValue.Function(paramList, hasVarArgs) ->
                        let paramList = [ for p in paramList -> 
                                                ProvidedParameter(p.Name.Replace("_","__").Replace(".", "_"),  typeof<obj>, optionalValue=null) ]
                        
                        let paramList = 
                            match hasVarArgs, paramList.Length with
                            | false, _ -> paramList
                            | true, 0  -> [ProvidedParameter("paramArray", typeof<obj[]>, isParamArray=true)]
                            // Since all arguments are optional, we cannot use ParamArray here
                            | true, _  -> List.append paramList [ProvidedParameter("paramArray", typeof<obj[]>, optionalValue=null)]

                        let paramCount = paramList.Length
                        
                        let pm = ProvidedMethod(
                                     methodName = memberName,
                                     parameters = paramList,
                                     returnType = typeof<SymbolicExpression>,
                                     IsStaticMethod = true,
                                     InvokeCode = fun args -> if args.Length <> paramCount then
                                                                failwithf "Expected %d arguments and received %d" paramCount args.Length
                                                 
                                                              if hasVarArgs then
                                                                let namedArgs = 
                                                                    Array.sub (Array.ofList args) 0 (paramCount-1)
                                                                    |> List.ofArray
                                                                let namedArgs = Quotations.Expr.NewArray(typeof<obj>, namedArgs)
                                                                let varArgs = args.[paramCount-1]
                                                                <@@ RInterop.call package name %%namedArgs %%varArgs @@>                                                 
                                                              else
                                                                let namedArgs = Quotations.Expr.NewArray(typeof<obj>, args)                                            
                                                                <@@ RInterop.call package name %%namedArgs [||] @@> )

                        pm.AddXmlDocDelayed (fun () -> match titles.Value.TryFind name with 
                                                       | Some docs -> docs 
                                                       | None -> "No documentation available")                                    
                        
                        yield pm :> MemberInfo

                        // Yield an additional overload that takes a Dictionary<string, object>
                        // This variant is more flexible for constructing lists, data frames etc.
                        let pdm = ProvidedMethod(
                                     methodName = memberName,
                                     parameters = [ ProvidedParameter("paramsByName",  typeof<IDictionary<string,obj>>) ],
                                     returnType = typeof<SymbolicExpression>,
                                     IsStaticMethod = true,
                                     InvokeCode = fun args -> if args.Length <> 1 then
                                                                failwithf "Expected 1 argument and received %d" args.Length
                                                              let argsByName = args.[0]
                                                              <@@   let vals = %%argsByName: IDictionary<string,obj>
                                                                    let valSeq = vals :> seq<KeyValuePair<string, obj>>
                                                                    RInterop.callFunc package name valSeq null @@> )
                        yield pdm :> MemberInfo                                    
                    | RValue.Value ->
                        yield ProvidedProperty(
                                propertyName = memberName,
                                propertyType = typeof<SymbolicExpression>,
                                IsStatic = true,
                                GetterCode = fun _ -> <@@ RInterop.call package name [| |] [| |] @@>) :> MemberInfo  ] )
                      
            this.AddNamespace(pns, [ pty ])

    // Expose all functions
    let ty = ProvidedTypeDefinition(asm, ns, "Session", Some(typeof<obj>))

    // add the type to the namespace
    do this.AddNamespace(ns, [ty])

[<TypeProviderAssembly>]
do()