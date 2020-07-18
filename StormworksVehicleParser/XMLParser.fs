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
  
  type private SysIOFile = System.IO.File
  
  
  /// Private data type used to represent a string part.
  /// The part can either be the character "=", i.e. the equality symbol,
  /// or it can be a word delineated by white space, or a text segment
  /// enclosed in quotation marks. We do not differentiate between words and
  /// other text segments; both are 'words' (AttrWord) to us.
  /// A list of RawAttributeStringPart can be used to represent the attribute
  /// string parts of the text found within a tag.
  /// For instance the attribute string parts of
  /// <MYTAG MyAttr= "Hello World!" LuckyNumber = 12> are
  /// 'MyAttr', =, 'Hello World!', LuckyNumber, = and '12'.
  type private RawAttributeStringPart =
    | AttrEqual
    | AttrWord of string
    
    /// Separate the first RawAttributeStringPart of a string from the rest of
    /// the string. The function is designed for use with
    /// ParseResult<_>.SplitToParts( ) .
    static member public PickStringPart (s: string) =
      let pos (s: string) (char: string) =
        if s.Contains char
         then s.IndexOf char
         else s.Length
      let tail (s: string) =
        (s.Substring 1).Trim ()
      if s = "" then ParseError "Attribute text was empty." else
      let c0 = char s.[0]
      if System.Char.IsWhiteSpace c0 then RawAttributeStringPart.PickStringPart (tail s) else
      if c0 = '=' then Result (AttrEqual, tail s) else
      if c0 = '"'
       then let rest = s.Substring 1
            let p = pos rest "\""
            if p < rest.Length
             then Result (AttrWord (rest.Substring (0,p)), rest.Substring (p+1))
             else ParseError "End of quote (\\\") missing!"
       else let pblank = pos s " "
            let pequal = pos s "="
            let pquote = pos s "\""
            let p = min pblank <| min pequal pquote
            Result (AttrWord (s.Substring (0,p)), s.Substring p)
  
  
  /// Private data type that represents an attribute key and value pair
  /// found within an XML tag.
  /// The attribute key and value pair is made from a triple of a key, an
  /// equality symbol and a value. The RawAttribute object represents the
  /// triple (AttrWord key)::(AttrEqual)::(AttrWord value) .
  type private RawAttribute =
    val public Name: string
    val public Value: string
    
    private new (key: string, value: string) =
      { Name= key; Value= value }
    
    /// Split text from within a tag into key value pairs.
    static member public SplitText (s: string) =
      let rec parsestringparts acc (parts: RawAttributeStringPart list) =
        match parts with
        | [] -> Result acc
        | AttrEqual::_ -> ParseError "Unexpected equality sign in attribute string."
        | (AttrWord key)::AttrEqual::(AttrWord value)::rest
          -> let ra = new RawAttribute (key, value)
             parsestringparts (ra::acc) rest
        | (AttrWord key)::_
          -> ParseError <| sprintf "Unexpected content of attribute string following key \"%s\"." key
      match ParseResult<_>.SplitToParts RawAttributeStringPart.PickStringPart "" s with
      | ParseError e -> ParseError e
      | Result b -> parsestringparts [] b
  
  
  /// A collection of attribute key and value pairs. The TagAttributes class is
  /// equipped with an indexer; it is a more user friendly collection than the
  /// otherwise mostly equivalent 'RawAttribute list'.
  type public TagAttributes () =
    let dict = new System.Collections.Generic.Dictionary<string,string> ()
    member private ta.Add k v = dict.Add (k, v)
    
    member public ta.Keys with get () = dict.Keys
    member public ta.Item with get k = dict.[k]
    member public ta.Exists k = dict.ContainsKey k
    
    /// Create a new attribute collection from the attribute text within a tag.
    static member public New (s: string) =
      match RawAttribute.SplitText s with
      | ParseError e -> ParseError e
      | Result attrs ->
          let tas = new TagAttributes ()
          let mutable collisionflag = false
          for ra in attrs
           do if tas.Exists ra.Name
               then collisionflag <- true
               else tas.Add ra.Name ra.Value
          if collisionflag
           then ParseError "Key collision in attributes."
           else Result tas
  
  
  /// A raw representation of an XML tag.
  /// The tag has a Name and owns an Attributes collection.
  /// The Attributes collection may well be empty.
  /// The tag is marked as either a new tag, an end tag or both.
  /// The RawTag has a public constructor but no other methods.
  type public RawTag =
    val public Name: string
    val public IsNewTag: bool
    val public IsEndTag: bool
    val public Attributes: TagAttributes
    
    /// Create and initialize a new RawTag from the text in between the angle
    /// brackets of an XML tag.
    /// First example: The text for the tag "</MyEndTag>" would be "/MyEndTag".
    /// Second example: The text for "<Hello Addressee = World/>" would be
    /// "Hello Addressee = World/"; the tag name is "Hello" and the tag owns
    /// one attribute – with attribute key "Addressee" and attribute value
    /// "World"; the tag is a new tag as well as (simultaniously) an end tag.
    public new (s: string) =
      let isnewtag, isendtag =
        match (s.StartsWith "/"), (s.EndsWith "/") with
        | false, false -> true, false // a new tag
        | true, false -> false, true // an end tag
        | false, true -> true, true // a new tag that is also an end tag (i.e. a single-tag)
        | true, true -> raise (ParseErrorException "Unable to parse tag of the form '</SOMETAG/>'.")
      let hasattribs = s.Contains " "
      let name =
        if hasattribs then let skip = if isnewtag then 0 else 1 in s.Substring (skip, (s.IndexOf " ") - skip)
        elif isnewtag then let skip = if isendtag then 1 else 0 in s.Substring (0, s.Length - skip)
        else s.Substring 1
      let attrtxt =
        if hasattribs
         then let rest1 = s.Substring ((s.IndexOf " ") + 1)
              let rest2 = if rest1.EndsWith "/"
                           then rest1.Substring (0, rest1.Length-1)
                           else rest1
              rest2.Trim ()
         else ""
      let attributes =
        match TagAttributes.New attrtxt with
        | ParseError m -> raise (ParseErrorException (sprintf "ParseError for tag '%s' while parsing attributes with message \"%s\"." name m))
        | Result attrs -> attrs
      { Name= name; IsNewTag= isnewtag; IsEndTag= isendtag; Attributes= attributes }
  
