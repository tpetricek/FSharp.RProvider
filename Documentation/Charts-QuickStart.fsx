﻿(** 
# Quickstart: Creating Charts

One of the compelling features of R is its ability to create beautiful charts.
With the R Type Provider, you can use all of R capabilities from F#, 
and create simple charts quickly to explore and visualize your data on-the-fly, 
as well as generate publication quality graphics that can be exported to virtually any format.

## Charts Basics

Basic charts can be found in the graphics package. 
Assuming you installed the R Type Provider in your project from NuGet, 
you can reference the required libraries and packages this way:
*)

#r @"..\packages\R.NET.1.5.5\lib\net40\RDotNet.dll"
#r @"..\packages\RDotNet.FSharp.0.1.2.1\lib\net40\RDotNet.FSharp.dll"
#r @"..\packages\R.NET.1.5.5\lib\net40\RDotNet.NativeLibrary.dll"
#r @"..\packages\RProvider.1.0.3\lib\RProvider.dll"

open System
open RDotNet
open RProvider
open RProvider.``base``
open RProvider.graphics

(**
Once the libraries and packages have been loaded, 
producing basic charts is as simple as this:
*)

let widgets = [ 3; 8; 12; 15; 19; 18; 18; 20; ]
let sprockets = [ 5; 4; 6; 7; 12; 9; 5; 6; ]

R.plot(widgets)

R.plot(widgets, sprockets)

R.barplot(widgets)

R.hist(sprockets)

R.pie(widgets)

(**
## Exporting and Saving Charts

Charts can be exported and saved to various formats; 
once you have opened the grDevices package, you can save a chart like this:
*)

// Required package to save charts
open RProvider.grDevices

// Create path to an image testimage.png on the Desktop
let desktop = Environment.GetFolderPath(Environment.SpecialFolder.Desktop)  
let path = desktop + @"\testimage.png"

// Open the device and create the file as a png.
// R.bmp, R.jpeg, R.pdf, ... will generate other formats.
R.png(filename=path, height=200, width=300, bg="white")
// Create the chart into the file
R.barplot(widgets)
// Close the device once the chart is complete
R.dev_off ()


(**
## Advanced Charts Options

The graphic functions exposed by the R Type Provider come in two flavors; 
they either have optional named arguments, 
followed by a ParamArray for extended arguments, 
or they take named parameters, an IDictionary<string,object> 
which contains all the arguments passed to the function.

### Named Arguments

Consider for instance the following example:
*)

R.barplot(widgets)
R.title(main="Widgets", xlab="Period", ylab="Quantity")

(**
R.title has 2 signatures, one of them with optional arguments, 
demonstrated above to set the main title as well as the labels for the x and y axis, 
ignoring some of the other available options. 
You can see another example in the previous section in the R.png call.

### Named Parameters

Named parameters allow you to specify every argument supported by R, 
as an IDictionary of string, object. 
The string is the name of the argument, and the object its value.

Finding the available arguments for a R function can be tricky;
the full list of arguments can usually be found in the 
[R developer documentation](http://stat.ethz.ch/R-manual/R-devel/library/),
navigating in the correct package. For instance, R.plot belongs to
graphics, and can be found 
[here](http://stat.ethz.ch/R-manual/R-devel/library/graphics/html/plot.html).

The easiest way to use that feature is to 
leverage the built-in function namedParams, like in this example:
*)

R.plot(
    namedParams [   
        "x", box widgets; 
        "type", box "o"; 
        "col", box "blue";
        "ylim", box [0; 25] ])

R.lines(
    namedParams [   
        "x", box sprockets; 
        "type", box "o"; 
        "pch", box 22;
        "lty", box 2;
        "col", box "red" ])

(**
The first call specifies what to plot (widgets), 
what type of line to use, the color, and the scale of the axis. 
The second call adds sprockets, specifying lty (the line type), 
and pch (the plotting character).

box is used to reduce all elements to objects, 
so that the lists have consistent types.

A possibly more elegant way to use namedParams is to follow the pattern below:
*)

namedParams [   
    "x", box widgets; 
    "type", box "o"; 
    "col", box "blue";
    "ylim", box [0; 25] ]
|> R.plot

namedParams [   
    "x", box sprockets; 
    "type", box "o"; 
    "pch", box 22;
    "lty", box 2;
    "col", box "red" ]
|> R.lines