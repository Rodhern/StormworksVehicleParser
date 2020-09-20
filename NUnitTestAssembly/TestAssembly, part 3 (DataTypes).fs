namespace ACTConsult.Rodhern.StormworksVehicleParser.NUnitTestAssembly
open NUnit.Framework
open System.Collections.Generic

open ACTConsult.Rodhern.StormworksVehicleParser


[< TestFixture; Category "DataTypes" >]
type KeyedCollectionTestClass () =
  
  let myKeyValueSource =
    let dict = new Dictionary<string,int list> ()
    ["one", [1]; "two", [2]; "three", [3];
     "single", [11];
     "solo", [12];
     "double", [13;14];
     "pair", [15;16]]
    |> List.iter dict.Add
    { new IKeyValueSource<int,string>
      with member src.GetKeys () = dict.Keys |> Set.ofSeq
           member src.GetValues key = dict.[key] }
  
  [< Test >]
  member test.CallTheFivePublicMethods () =
    let n = myKeyValueSource.GetKeys () |> Set.count
    let kc = new KeyedCollection<int> (myKeyValueSource)
    Assert.AreEqual (n, kc.NumberOfKeysLeft (), "Expected all {0} keys to be 'untouched'.", n)
    let single = kc.Get "single"
    let solo = kc.Get "solo"
    let n = n - 2
    Assert.AreEqual (n, kc.NumberOfKeysLeft (), "Expected {0} remaining keys to be 'untouched'.", n)
    let noval = kc.GetOptionalSingleOrMultiple "missing key"
    Assert.AreEqual (n, kc.NumberOfKeysLeft (), "Expected {0} remaining keys after looking up a missing key to be 'untouched'.", n)
    let pair = kc.GetOptionalSingleOrMultiple "pair"
    let n = n - 1
    Assert.AreEqual (n, kc.NumberOfKeysLeft (), "Expected {0} remaining keys to be 'untouched'.", n)
    let ignoren0 = kc.Ignore ["missing key"]
    let ignoren1 = kc.Ignore ["double"]
    let n = n - 1
    Assert.AreEqual (0, ignoren0, "Ignoring key 'missing key' should have no effect.")
    Assert.AreEqual (1, ignoren1, "Ignoring key 'double' should ignore exactly one key.")
    Assert.AreEqual (n, kc.NumberOfKeysLeft (), "Expected {0} remaining keys to be 'untouched'.", n)
    let onelist = kc.GetOptionalSingleOrMultiple "one"
    let n = n - 1
    Assert.AreEqual (n, kc.NumberOfKeysLeft (), "Expected {0} remaining keys to be 'untouched'.", n)
    let ignoren2 = kc.Ignore ["one"; "two"; "three"]
    let n = n - 2
    Assert.AreEqual (2, ignoren2, "Ignoring three keys of which one was already fetched should return that two keys where ignored.")
    let verificationresult = kc.VerifyNoKeysLeft ()
    Assert.AreEqual (Result (), verificationresult, "Verification that no keys are left untouched should succeed.")
    Assert.AreEqual (0, kc.NumberOfKeysLeft (), "Expected no remaining 'untouched' keys (n = {0}).", n)
    // Check that the expected results (values) where retrieved.
    Assert.AreEqual (Result 11, single, "Check for fetched value(s) for '{0}'.", "single")
    Assert.AreEqual (Result 12, solo, "Check for fetched value(s) for '{0}'.", "solo")
    Assert.AreEqual (Result [15;16], pair, "Check for fetched value(s) for '{0}'.", "pair")
    Assert.AreEqual (Result [1], onelist, "Check for fetched value(s) for '{0}'.", "one")
  
  [< Test >]
  member test.CheckSomeFailures () =
    let isParseError = function | Result _ -> false | ParseError _ -> true
    let kc = new KeyedCollection<int> (myKeyValueSource)
    let failresult1 = kc.Get "missing key"
    let failresult2 = kc.VerifyNoKeysLeft ()
    let _ = kc.GetOptionalSingleOrMultiple "three"
    let failresult3 = kc.Get "three"
    let failresult4 = kc.VerifyNoKeysLeft ()
    Assert.IsTrue (isParseError failresult1, "The keyed collection should not return a result for a missing key.")
    Assert.IsTrue (isParseError failresult2, "The keyed collection should, at this point, have plenty of 'untouched' keys left.")
    Assert.IsTrue (isParseError failresult3, "The keyed collection should not return a result for an already 'used' key.")
    Assert.IsTrue (isParseError failresult4, "The keyed collection should, at this point, have plenty of 'untouched' keys left.")
  
