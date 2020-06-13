namespace ACTConsult.Rodhern.StormworksVehicleParser.NUnitTestAssembly
open NUnit.Framework


[< TestFixture >]
type FirstTestClass () =
  
  [< Test >]
  member test.FirstTest () =
    Assert.Pass "A simple test case that will always pass!"
