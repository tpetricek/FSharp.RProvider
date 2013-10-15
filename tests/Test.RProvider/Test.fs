module Test.RProvider

open RDotNet
open RDotNet.Internals
open RProvider
open RProvider.RInterop
open RProvider.``base``
open System
open FsCheck
open NUnit.Framework
open System.Numerics
open System.Text

// Generic function to test that a value round-trips
// when SEXP is asked for the value by-type
let testRoundTrip (x: 'a) (typeof: SymbolicExpressionType) (clsName: Option<string>) =
    let sexp = toR(x)
    Assert.AreEqual(x, sexp.GetValue<'a>())
    Assert.AreEqual(sexp.Type, typeof)
    Assert.AreEqual(sexp.Class, Option.toArray clsName)

// Generic function to test that a value round-trips
// when SEXP is asked for the value by-type, and
// as the default .NET representation
let testRoundTripAndDefault (x: 'a) (typeof: SymbolicExpressionType) (clsName: Option<string>) =
    testRoundTrip x typeof clsName
    let sexp = toR(x)
    Assert.AreEqual(x, unbox<'a> sexp.Value)    

let testVector (xs: 'scalarType[]) (typeof: SymbolicExpressionType) (clsName: Option<string>) =    
    // Test arrays and lists round-trip
    testRoundTrip (Array.toList xs) typeof clsName
    // Array is the default return type from .Value
    testRoundTripAndDefault xs typeof clsName
    // Can only round-trip a vector as a scalar if it is of length 1
    if xs.Length <> 1 then
        ignore <| Assert.Throws<InvalidOperationException>(fun () -> toR(xs).GetValue<'scalarType>() |> ignore)


let testScalar (x: 'scalarType) (typeof: SymbolicExpressionType) (clsName: Option<string>) = 
    // Scalars round-trip to scalar when scalar-type is requested explicitly
    testRoundTrip x typeof clsName

    // Scalars round-trip as vectors
    let sexp = toR(x)
    Assert.AreEqual([|x|], unbox<'scalarType[]>(sexp.Value))
    Assert.AreEqual([|x|], sexp.GetValue<'scalarType[]>())

[<Test>]
let ``Date vector round-trip tests``() = 
  Check.QuickThrowOnFailure(fun (xs: DateTime[]) ->
    testVector xs SymbolicExpressionType.NumericVector (Some "Date"))

[<Test>]
let ``Date scalar round-trip tests`` () = 
  Check.QuickThrowOnFailure(fun (x: DateTime) ->
    testScalar x SymbolicExpressionType.NumericVector (Some "Date"))

[<Test>]
let ``Int vector round-trip tests`` () =
  Check.QuickThrowOnFailure(fun (xs: int[]) ->
    testVector xs SymbolicExpressionType.IntegerVector None)

[<Test>]
let ``Int scalar round-trip tests`` () =
  Check.QuickThrowOnFailure(fun (x: int) ->
    testScalar x SymbolicExpressionType.IntegerVector None)

[<Test>]
let ``Double vector round-trip tests`` () =
  Check.QuickThrowOnFailure(fun (xs: double[]) ->
    testVector xs SymbolicExpressionType.NumericVector None)

[<Test>]
let ``Double scalar round-trip tests`` () =
  Check.QuickThrowOnFailure(fun (x: double) ->
    testScalar x SymbolicExpressionType.NumericVector None)

[<Test>]
let ``Bool vector round-trip tests`` () =
  Check.QuickThrowOnFailure(fun (xs: bool[]) ->
    testVector xs SymbolicExpressionType.LogicalVector None)

[<Test>]
let ``Bool scalar round-trip tests`` () = 
  Check.QuickThrowOnFailure(fun (x: bool) ->
    testScalar x SymbolicExpressionType.LogicalVector None)

[<Test>]
let ``Complex vector round-trip tests`` () =
  Check.QuickThrowOnFailure(fun (ris: (double*double)[]) ->
    let xs = [| for (r,i) in ris do
                    if not(Double.IsNaN(r) || Double.IsNaN(i)) then
                        yield Complex(r, i) |] 
    testVector xs SymbolicExpressionType.ComplexVector None)

[<Test>]
let ``Complex scalar round-trip tests`` () =
  Check.QuickThrowOnFailure(fun (r: double) (i: double) ->
    if not(Double.IsNaN(r) || Double.IsNaN(i)) then
        let x = Complex(r, i) 
        testScalar x SymbolicExpressionType.ComplexVector None)

//[<Test>]
// Has various issues - embedded nulls, etc.
let ``String arrays round-trip``() =
  Check.QuickThrowOnFailure(fun (strings: string[]) ->
    // We only want to test for ASCII strings
    if Array.forall (fun s -> s = Encoding.ASCII.GetString(Encoding.ASCII.GetBytes(s))) strings then
        let sexp = toR(strings)

        Assert.AreEqual(strings, unbox<string[]> sexp.Value)
        Assert.AreEqual(strings, sexp.GetValue<string[]>()) )

//[<Test>]
// Has various issues - embedded nulls, etc.
let ``Strings round-trip``() =
  Check.QuickThrowOnFailure(fun (value: string) ->
    // We only want to test for ASCII strings
    //if Array.forall (fun s -> s = Encoding.ASCII.GetString(Encoding.ASCII.GetBytes(s))) strings then
        let sexp = toR(value)    
        Assert.AreEqual(value, sexp.GetValue<string>()))

let roundTripAsFactor (value:string[]) = 
    let sexp = R.as_factor(value)
    Assert.AreEqual(value, sexp.GetValue<string[]>())

let roundTripAsDataframe (value: string[]) = 
    let df = R.data_frame(namedParams [ "Column", value ]).AsDataFrame()    
    Assert.AreEqual(value, df.[0].GetValue<string[]>())

[<Test>]
let ``String arrays round-trip via factors`` () = 
    roundTripAsFactor [| "foo"; "bar"; "foo"; "bar" |]

[<Test>]
let ``String arrays round-trip via DataFrame`` () = 
    roundTripAsDataframe [| "foo"; "bar"; "foo"; "bar" |]
