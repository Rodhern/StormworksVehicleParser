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
    /// "World"; the tag is a new tag as well as (simultaneously) an end tag.
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
        if not hasattribs
         then ""
         else let rest1 = s.Substring ((s.IndexOf " ") + 1)
              let rest2 = if rest1.EndsWith "/"
                           then rest1.Substring (0, rest1.Length-1)
                           else rest1
              rest2.Trim ()
      let attributes =
        match TagAttributes.New attrtxt with
        | ParseError m -> raise (ParseErrorException (sprintf "ParseError for tag '%s' while parsing attributes with message \"%s\"." name m))
        | Result attrs -> attrs
      { Name= name; IsNewTag= isnewtag; IsEndTag= isendtag; Attributes= attributes }
  
  
    /// An XML text is made up of two ingredients. The main ingredient is tags.
    /// The other ingredient is entirely optional; it is text delimited by the
    /// inner most tags. To help tell text fragments that are the content of
    /// the inner most tags apart from text strings in general, the text
    /// fragments are given the string alias 'RawContentText'. The alias is in
    /// turn one of the two discriminated union options of a 'RawPart'; the
    /// other option being a 'RawTag'.
    type public RawContentText = string
    
    
    /// The assumption is that an XML text is made up entirely of just two
    /// ingredients – RawTag parts and RawContentText parts.
    /// The goal of the 'RawPart' class is to be able to turn a string of
    /// characters into a string of RawParts (i.e. a 'RawPart list').
    type public RawPart =
      | RawTagPart of RawTag
      | RawContentTextPart of RawContentText
      with
        
        /// Identify where to cut to get the first XML part (RawPart) of the
        /// string, then return that part and the remaining string.
        /// The cut is always done at an angle bracket character.
        static member public PickRawPart (s: string) =
          let pickTag (s: string) =
            if s.Length < 3 then ParseError "Text is not long enough to be a tag." else
            if not (s.StartsWith "<") then ParseError "Character '<' expected!" else
            if not (s.Contains ">") then ParseError "Character '>' expected!" else
            let endpos = s.IndexOf ">"
            let tagtext = s.Substring (1, endpos-1)
            let resttext = s.Substring (endpos+1)
            let rawtag = RawTagPart (new RawTag (tagtext.Trim ()))
            Result (rawtag, resttext.Trim ())
          let pickContentText (s: string) =
            if not (s.Contains "<") then ParseError "Character '<' expected!" else
            let endpos = s.IndexOf "<"
            let textpart = s.Substring (0, endpos)
            let resttext = s.Substring (endpos)
            let rawcontent = RawContentTextPart (textpart.Trim ())
            Result (rawcontent, resttext)
          let s = s.Trim ()
          if s.StartsWith "<"
           then pickTag s
           else pickContentText s


    /// In its purest form the TagNode represent a raw tag marking the start of
    /// a node, a matching rawtag marking the end of the node, and the XML raw
    /// parts found in between.
    /// When the raw tag marking the start of the node is also marking the end
    /// of the node (i.e. a single-tag) then the TagNode represents just that
    /// one raw tag.
    /// There are three possibilities for the kind of XML raw parts that are
    /// found between the new raw tag and the matching end raw tag:
    /// There may be nothing in between the tags – which is functionally
    /// similar to a single-tag.
    /// There may be a text fragment – which is then the RawContentText value
    /// for the node.
    /// The last possibility is that there are child nodes in between the tags
    /// – which themselves are represented as TagNode objects.
    /// The RawContentText text fragment option and the TagNode child nodes
    /// option are mutually exclusive.
    type TagNode =
      val public Tag: RawTag
      val public ContentText: RawContentText option
      val public Children: TagNode list
      public new (tag: RawTag, txtcontent: RawContentText option, children: TagNode list) =
        if txtcontent.IsSome && (not children.IsEmpty)
         then raise (ParseErrorException "A tag may have a content text or child tags but not both.")
        { Tag= tag; ContentText= txtcontent; Children= children }
      override tn.ToString () =
        match tn.ContentText with
        | Some s
          -> sprintf "{Tag: \"%s\"; Child count: %d; Content text: \"%s\"}" tn.Tag.Name tn.Children.Length s
        | None
          -> match tn.Children.Length with
             | 0 -> sprintf "{Tag: \"%s\"}" tn.Tag.Name
             | 1 -> sprintf "{Tag: \"%s\"; Child: \"%s\"}" tn.Tag.Name tn.Children.Head.Tag.Name
             | n -> sprintf "{Tag: \"%s\"; Child count: %d}" tn.Tag.Name tn.Children.Length
      
      /// Initialize a keyed collection with the tag attributes of the raw tag.
      /// The tag attributes are string values.
      member public node.CollectAttributes () =
        { new IKeyValueSource<string,string> with
           member self.GetKeys () = Set node.Tag.Attributes.Keys
           member self.GetValues (key: string) = [node.Tag.Attributes.[key]]
        } |> KeyedCollection
      
      /// Initialize a keyed collection of child tag nodes.
      /// The collection is a keyed shallow copy of the Children property.
      member public node.CollectChildTags () =
        { new IKeyValueSource<_,_> with
           member self.GetKeys () =
             node.Children
             |> List.map (fun c -> c.Tag.Name)
             |> Set
           member self.GetValues (key: string) =
             node.Children
             |> List.filter (fun c -> c.Tag.Name = key)
        } |> KeyedCollection
      
      /// Given a list of raw parts, i.e. a list of raw tags and content text
      /// fragments, select enough raw parts to create a TagNode. The result
      /// of the function is the TagNode created from the selected raw parts
      /// along with the remainder list of raw parts not used for the TagNode.
      /// PickPartsForTag is used with ParseResult.SplitToParts to turn a
      /// RawPart list into a TagNode list. When used successfully on an entire
      /// XML text the resulting list contains exactly one TagNode – the root
      /// node.
      static member public PickPartsForTag (partlist: RawPart list) =
        /// Split the list of parts into three bits. The first bit is the list
        /// of parts up until, but excluding, the end tag for the given tag
        /// name. The second bit is just the end tag matching the given tag
        /// name. The last bit is the tail of the parts list beyond said end
        /// tag.
        let rec splitAtEndTag (tagname: string) (parts: RawPart list) =
          let rec iterate (acc: RawPart list) (rest: RawPart list): RawPart list * RawPart * RawPart list =
            match rest with
            | ((RawContentTextPart _) as part)::tail
              -> iterate (part::acc) tail
            | (RawTagPart tag)::_ when tag.Name = tagname && tag.IsNewTag
              -> raise (ParseErrorException (sprintf "Tag (named \"%s\") within same named tag is (usually) not allowed." tagname))
            | ((RawTagPart tag) as raw)::tail when tag.Name = tagname && tag.IsEndTag
              -> if tag.Attributes.Keys.Count > 0
                  then raise (ParseErrorException "End tags are (usually) not allowed to have attributes.")
                 List.rev acc, raw, tail
            | ((RawTagPart tag) as raw)::tail
              -> if not tag.IsNewTag
                  then raise (ParseErrorException <| sprintf "Encountered orphaned end tag \"%s\"." tag.Name)
                 elif tag.IsEndTag
                  then iterate (raw::acc) tail
                 else
                  let subacc, endtag, subtail = splitAtEndTag tag.Name tail
                  iterate (endtag::(List.rev subacc)@(raw::acc)) subtail
            | _ -> raise (ParseErrorException (sprintf "End tag for '%s' not found." tagname))
          iterate [] parts
        /// Use splitAtEndTag and recursively call PickPartsForTag to build
        /// child nodes. Once recursion is done return the TagNode made from
        /// the head tag. The child nodes are the 'Children' (grandchildren and
        /// so on) of the returned TagNode.
        let buildNode (headtag: RawTag) (restlist: RawPart list) =
          if not headtag.IsNewTag
           then raise (ParseErrorException (sprintf "A new tag is needed to build a new node; tag named '%s' was not a new tag." headtag.Name))
          elif headtag.IsEndTag // i.e. a single-tag
           then let node = new TagNode (headtag, None, [])
                in Result (node, restlist)
          else
          let contenttxt, restlist =
            match restlist with
            | (RawContentTextPart ctxt)::tail -> Some ctxt, tail
            | _ -> None, restlist
          let innerparts, _, restlist =
            splitAtEndTag headtag.Name restlist
          let children =
            match ParseResult<_>.SplitToParts TagNode.PickPartsForTag [] innerparts with
            | ParseError m -> raise (ParseErrorException <| sprintf "Failed to parse child tags with error message \"%s\"" m)
            | Result childtagnodes -> childtagnodes
          Result (TagNode (headtag, contenttxt, children), restlist)
        // Pass head tag and tail to buildNode and catch any ParseErrorException thrown
        match partlist with
        | (RawTagPart tag)::tail
          -> try
               buildNode tag tail
              with | ParseErrorException m -> ParseError m
        | part::_ -> ParseError <| sprintf "Unable to build TagNode from RawPart '%A'." part
        | [] -> ParseError "Unable to build TagNode; the part list was empty."
  
