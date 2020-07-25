namespace ACTConsult.Rodhern.StormworksVehicleParser.NUnitTestAssembly
open NUnit.Framework

open ACTConsult.Rodhern.StormworksVehicleParser
open ACTConsult.Rodhern.StormworksVehicleParser.XML


[< TestFixture; Category "XML" >]
type RawTagTestClass () =
  
  let (=?>) (tagtxt: string) args = // infix short hand notation for CheckRawTag
    RawTagTestClass.CheckRawTag (new RawTag (tagtxt)) args
  
  static member public CheckRawTag (tag: RawTag) (name: string, isNewTag: bool, isEndTag: bool, attribs: (string * string) list) =
    Assert.AreEqual (name, tag.Name, "The tag name differed.")
    Assert.AreEqual (isNewTag, tag.IsNewTag, "The 'IsNewTag' property of tag '{0}' was incorrect.", name)
    Assert.AreEqual (isEndTag, tag.IsEndTag, "The 'IsEndTag' property of tag '{0}' was incorrect.", name)
    CollectionAssert.AreEquivalent (List.map fst attribs, tag.Attributes.Keys, "The attribute keys of tag '{0}' differed.", name)
    for key, value in attribs
     do Assert.AreEqual (value, tag.Attributes.[key], "Value for key '{0}' of tag '{1}' differed.", key, name)
  
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


type RawPartExpectedValue =
  /// F-sharp does allow naming the individual parts of a tuple used in a
  /// discriminated union. In this case the compiler is not too happy doing
  /// pattern matching afterwards though. Hence this work-around.
  /// The arguments are 'name', 'isNewTag', 'isEndTag' and 'attribs'.
  | TagPartValue of (string * bool * bool * (string * string) list)
  | TextPartValue of content: RawContentText
  with
    
    member public v.AssertAreEqual (part: RawPart) =
      match v, part with
      | TagPartValue tagparams, RawTagPart tag
        -> RawTagTestClass.CheckRawTag tag tagparams
      | TextPartValue expstr, RawContentTextPart actstr
        -> Assert.AreEqual (expstr, actstr, "The text content differed.")
      | TagPartValue _, RawContentTextPart _
        -> Assert.Fail "Expected a tag part but found a text content part."
      | TextPartValue _, RawTagPart _
        -> Assert.Fail "Expected a text content part but found a tag part."
    
    static member public CollectionAssertAreEqual (expected: RawPartExpectedValue list, actual: RawPart list) =
      Assert.AreEqual (expected.Length, actual.Length, "The list of raw parts did not have the expected length.")
      let checkpair (v: RawPartExpectedValue) = v.AssertAreEqual
      List.iter2 checkpair expected actual


[< TestFixture; Category "XML" >]
type RawPartTestClass () =
  
  let unwrap = function | Result result -> result | ParseError msg -> raise (ParseErrorException msg)
  
  static member public TestExample1Source () = """
    <MyRoot><SomeItemWithoutSpaces/><SomeOtherItemWithoutSpaces></SomeOtherItemWithoutSpaces>
     <FirstItem>
      <Child/>
      <Child>
       <GrandChild Name= "Alice"/>
       <GrandChild Name= "Bob"/>
      </Child>
     </FirstItem>
     <SecondItem>Just a text fragment.</SecondItem>
     <ThirdItem Desc= "Squares" 1=1 2=4 3=9 4=16 5=25></ThirdItem>
    </MyRoot>"""
  
  static member public TestExample1Parts () = [
    ("MyRoot", true, false, []) |> TagPartValue
    ("SomeItemWithoutSpaces", true, true, []) |> TagPartValue
    ("SomeOtherItemWithoutSpaces", true, false, []) |> TagPartValue
    ("SomeOtherItemWithoutSpaces", false, true, []) |> TagPartValue
    ("FirstItem", true, false, []) |> TagPartValue
    ("Child", true, true, []) |> TagPartValue
    ("Child", true, false, []) |> TagPartValue
    ("GrandChild", true, true, ["Name","Alice"]) |> TagPartValue
    ("GrandChild", true, true, ["Name","Bob"]) |> TagPartValue
    ("Child", false, true, []) |> TagPartValue
    ("FirstItem", false, true, []) |> TagPartValue
    ("SecondItem", true, false, []) |> TagPartValue
    TextPartValue "Just a text fragment."
    ("SecondItem", false, true, []) |> TagPartValue
    ("ThirdItem", true, false, ["Desc","Squares"; "1","1"; "2","4"; "3","9"; "4","16"; "5","25"]) |> TagPartValue
    ("ThirdItem", false, true, []) |> TagPartValue
    ("MyRoot", false, true, []) |> TagPartValue ]
  
  [< Test >]
  member test.PickRawPartTest () =
    let source = RawPartTestClass.TestExample1Source ()
    let expected = RawPartTestClass.TestExample1Parts ()
    let result = ParseResult.SplitToParts RawPart.PickRawPart "" source |> unwrap
    RawPartExpectedValue.CollectionAssertAreEqual (expected, result)


[< TestFixture; Category "XML" >]
type TagNodeTestClass () =
  
  let unwrap = function | Result result -> result | ParseError msg -> raise (ParseErrorException msg)
  
  static member public AssertTagNodesAreEqual (expected: TagNode, actual: TagNode) =
    // Extract expected tag values from expected node.
    let name = expected.Tag.Name
    let isNew = expected.Tag.IsNewTag
    let isEnd = expected.Tag.IsEndTag
    let attribs = [ for key in expected.Tag.Attributes.Keys do yield (key, expected.Tag.Attributes.[key]) ]
    // Check that the raw tag matches expectations.
    RawTagTestClass.CheckRawTag actual.Tag (name, isNew, isEnd, attribs)
    // Check that the content text (if any) matches expectations.
    match expected.ContentText, actual.ContentText with
    | None, None -> () // i.e. indeed equal
    | Some _, None -> Assert.Fail <| sprintf "Expected text content for tag '%s' not found." name
    | None, Some _ -> Assert.Fail <| sprintf "Tag '%s' was not expected to own text content." name
    | Some exp, Some act -> Assert.AreEqual (exp, act, "The text content of tag '{0}' differed.", name)
    // Check recursively that all child nodes also match expectations.
    Assert.AreEqual (expected.Children.Length, actual.Children.Length, "The number of child nodes for tag '{0}' differed.", name)
    let checkpair e a = TagNodeTestClass.AssertTagNodesAreEqual (e, a)
    List.iter2 checkpair expected.Children actual.Children
  
  [< Test >]
  member test.NodeTreeTest () =
    // Create some 'expected' tags to be used for nodes in the node tree.
    let root = new RawTag "MyRoot"
    let someitem1 = new RawTag "SomeItemWithoutSpaces/"
    let someitem2 = new RawTag "SomeOtherItemWithoutSpaces"
    let firstitem = new RawTag "FirstItem"
    let child1 = new RawTag "Child/"
    let child2 = new RawTag "Child"
    let grandchild1 = new RawTag "GrandChild Name= \"Alice\"/"
    let grandchild2 = new RawTag "GrandChild Name= \"Bob\"/"
    let seconditem = new RawTag "SecondItem"
    let thirditem = new RawTag "ThirdItem Desc=Squares 1=1 2=4 3=9 4=16 5=25"
    // Turn the 'expected' tags into 'expected' nodes.
    let someitem1node = new TagNode (someitem1, None, [])
    let someitem2node = new TagNode (someitem2, None, [])
    let firstitemnode =
      let child1node = new TagNode (child1, None, [])
      let grandchild1node = new TagNode (grandchild1, None, [])
      let grandchild2node = new TagNode (grandchild2, None, [])
      let child2node = new TagNode (child2, None, [grandchild1node; grandchild2node])
      new TagNode (firstitem, None, [child1node; child2node])
    let seconditemnode = new TagNode (seconditem, Some "Just a text fragment.", [])
    let thirditemnode = new TagNode (thirditem, None, [])
    let children = [ someitem1node; someitem2node; firstitemnode; seconditemnode; thirditemnode ]
    let expected = new TagNode (root, None, children)
    // Create the root TagNode straight from xml source and check the result.
    let xmlsource = RawPartTestClass.TestExample1Source ()
    let sourceparts = ParseResult.SplitToParts RawPart.PickRawPart "" xmlsource |> unwrap
    let result =
      match ParseResult.SplitToParts TagNode.PickPartsForTag [] sourceparts |> unwrap with
      | [] ->raise (ParseErrorException "Parsing the XML example did not produce a TagNode.")
      | [node] -> node
      | _ -> raise (ParseErrorException "The XML example produced more than one TagNode; only one was expected.")
    TagNodeTestClass.AssertTagNodesAreEqual (expected, result)
