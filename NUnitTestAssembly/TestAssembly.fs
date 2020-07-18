namespace ACTConsult.Rodhern.StormworksVehicleParser.NUnitTestAssembly
open NUnit.Framework

// The unit tests are at the same time usage examples that illustrate how some
// of the more alien functions work.

#nowarn "25" // i.e. no compiler warnings for incomplete pattern matches.


open ACTConsult.Rodhern.StormworksVehicleParser

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


open ACTConsult.Rodhern.StormworksVehicleParser.XML

[< TestFixture; Category "XML" >]
type RawTagTestClass () =
  
  let checkRawTag (tag: RawTag) (name: string, isNewTag: bool, isEndTag: bool, attribs: (string * string) list) =
    Assert.AreEqual (name, tag.Name, "The tag name differed.")
    Assert.AreEqual (isNewTag, tag.IsNewTag, "The 'IsNewTag' property was incorrect.")
    Assert.AreEqual (isEndTag, tag.IsEndTag, "The 'IsEndTag' property was incorrect.")
    CollectionAssert.AreEquivalent (List.map fst attribs, tag.Attributes.Keys, "The attribute keys differed.")
    for key, value in attribs
     do Assert.AreEqual (value, tag.Attributes.[key], "Value for key '{0}' differed.", key)
  
  let (=?>) (tagtxt: string) args = // infix short hand notation for checkRawTag
    checkRawTag (new RawTag (tagtxt)) args
  
  [< Test >] /// Currently empty tags are considered valid.
  member test.TestEmptyTag () =
    let emptyresult = ("", true, false, [])
    "" =?> emptyresult
    " " =?> emptyresult
    "  " =?> emptyresult
  
  [< Test >] /// Some invalid tag texts that will lead to parse error exceptions.
  member test.TestInvalidTag () =
    let test (invalidexpstr: string) =
      let emptyresult = ("", true, false, [])
      let testdelegate = new TestDelegate (fun () -> invalidexpstr =?> emptyresult)
      let errormessage = "The tag text was expected to lead to a ParseErrorException."
      Assert.Throws(typeof<ParseErrorException>, testdelegate, errormessage) |> ignore
    test "/"
    test "/INVALIDSINGLETAG/"
    test "UNPARSABLE TAG"
  
  [< Test >] /// Tag: <HELLO> .
  member test.TestSimpleNewTag () =
    "HELLO" =?>
     ("HELLO", true, false, [])
  
  [< Test >] /// Tag: </HELLO> .
  member test.TestSimpleEndTag () =
    "/HELLO" =?>
     ("HELLO", false, true, [])
  
  [< Test >] /// Tag: <HELLO/> .
  member test.TestSingleTag () =
    "HELLO/" =?>
     ("HELLO", true, true, [])
  
  [< Test >] /// Tag: <Hello Addressee = World/> .
  member test.TestSimpleAttribute () =
    "Hello Addressee = World/" =?>
     ("Hello", true, true, ["Addressee", "World"])
  
  [< Test >] /// Tag: <QVAL Key="My Value"> .
  member test.TestQuotedAttributeValue () =
    "QVAL Key=\"My Value\"" =?>
     ("QVAL", true, false, ["Key", "My Value"])
  
  [< Test >] /// Tag: <QKEY "My Key" = some_value> .
  member test.TestQuotedAttributeKey () =
    "QKEY \"My Key\" = some_value" =?>
     ("QKEY", true, false, ["My Key", "some_value"])
  
  [< Test >] /// Tag: <LIST One=1 Three = 3 Four= 4  Two = 2/> .
  member test.TestAttributeList () =
    "LIST One=1 Three = 3 Four= 4  Two = 2/" =?>
     ("LIST", true, true, ["One","1"; "Two","2"; "Three","3"; "Four","4"])
  
