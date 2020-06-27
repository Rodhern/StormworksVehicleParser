// Stormworks Vehicle Parser, a third party tool.
// Copyright (c) 2020, Robert Nielsen. All Rights Reserved.
// See License.txt in the repository root for license information.
namespace ACTConsult.Rodhern.StormworksVehicleParser.XML
open ACTConsult.Rodhern.StormworksVehicleParser
open System.Collections.Generic
open System
  
  // The standard libraries for XML handling are not all that happy simply
  // taking a text (a string or a text file) and start chopping it into parts.
  // The reason most likely is that the libraries are designed to work with
  // fairly complex situations (e.g. comments and quotes in the XML).
  // However, there is nothing stopping us from making our own text chopping
  // functionality, and we do not care about parse performance, so basic
  // string manipulation will do just fine.
  
  type private TODO = System.Object
