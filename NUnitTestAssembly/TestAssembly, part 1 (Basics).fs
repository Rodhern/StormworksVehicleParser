namespace ACTConsult.Rodhern.StormworksVehicleParser.NUnitTestAssembly
open NUnit.Framework

open ACTConsult.Rodhern.StormworksVehicleParser

#nowarn "25" // i.e. no compiler warnings for incomplete pattern matches.

// The unit tests are at the same time usage examples that illustrate how some
// of the more alien functions work.


[< TestFixture; Category "Basics" >]
type ParseResultTestClass () =
  
  let unwrap = function Result result -> result
  
  [< Test >]
  member test.FolderTest () =
    let xs = [1;2;3;4]
    let map x = Result (x.ToString ())
    let folder = ParseResult.Folder map
    let state0 = Result []
    let result = List.foldBack folder xs state0 |> unwrap
    let expected = ["1";"2";"3";"4"]
    CollectionAssert.AreEqual (expected, result)
  
  [< Test >]
  member test.MapEachTest () =
    let xs = [1;2;3;4]
    let map x = Result (x.ToString ())
    let result = ParseResult.MapEach map xs |> unwrap
    let expected = ["1";"2";"3";"4"]
    CollectionAssert.AreEqual (expected, result)
  
  [< Test >]
  member test.SplitToPartsTest () =
    let xs = [1;2;3;4]
    let pickhead = function head::tail -> Result (head.ToString (), tail)
    let emptyval = []
    let result = ParseResult<string>.SplitToParts pickhead emptyval xs |> unwrap
    let expected = ["1";"2";"3";"4"]
    CollectionAssert.AreEqual (expected, result)
