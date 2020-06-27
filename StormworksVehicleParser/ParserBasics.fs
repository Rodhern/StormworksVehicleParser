// Stormworks Vehicle Parser, a third party tool.
// Copyright (c) 2020, Robert Nielsen. All Rights Reserved.
// See License.txt in the repository root for license information.
namespace ACTConsult.Rodhern.StormworksVehicleParser
  
  
  /// The result produced by a parser is returned as a ParseResult.
  /// Most of the time the parser succeeds and returns "Result of 'T".
  /// The "Result of 'T" carries the actual result as its parameter.
  /// If the parse attempt fails a ParseError is returned instead.
  /// The "ParseError" holds an error message string as its parameter.
  type public 'a ParseResult =
    | ParseError of string
    | Result of 'a
    with
      
      /// Combine an argument value that is itself a ParseResult (of 'x) and
      /// apply a map function to the argument to get a new ParseResult (of 'a).
      /// The map function must itself return a ParseResult.
      /// When the argument value is "Result x" returns "map x".
      /// If the argument value is "ParseError" returns a "ParseError".
      static member public Map (arg: 'x ParseResult, map: 'x -> 'a ParseResult) =
        match arg with
        | ParseError e -> ParseError e
        | Result x -> map x
      
      /// Given a map and a list of arguments applies the map to each argument.
      /// The map function must itself return a ParseResult.
      /// If all arguments are mapped successfully returns a result,
      /// "Result of ('a list)", with the individual results in a list.
      /// Otherwise, if any of the arguments fail to map, MapEach will
      /// return the ParseError of the first failure.
      static member public MapEach (map: 'x -> 'a ParseResult) (xs: 'x list) =
        let rec iterate acc xs =
          match xs with
          | [] -> Result (List.rev acc)
          | x::tail
            -> match map x with
               | ParseError m -> ParseError m
               | Result fx -> iterate (fx::acc) tail
        iterate [] xs
      
      /// The Folder is useful when used with the sequence foldBack methods.
      /// Folder is given a map that maps an element x to a ParseResult,
      /// and is further given an element, x, and an accumulator state.
      /// The accumulator state is a list of accumulated results, wrapped in a
      /// ParseResult.Result; the initial state is often "(Result [])".
      /// Folder then maps the element x and appends the result as the new
      /// head of the accumulated list of results.
      /// The just extended list is wrapped as a ParseResult.Result and is
      /// the new accumulator state, which is the result of Folder.
      static member public Folder (map: 'x -> 'a ParseResult) x acc =
        match acc with
        | ParseError _ -> acc
        | Result tail
          -> match map x with
             | ParseError e -> ParseError e
             | Result k -> Result (k::tail)
      
      /// Chop a stream into a list of its parts.
      /// SplitToParts takes a stream input as its last argument.
      /// The stream may be a string, a list, a sequence or anything else that
      /// represents some unprocessed data.
      /// The first argument to SplitToParts is itself a function, 'pickhead'.
      /// The 'pickhead' function will, when given a stream, identify the
      /// first element of the stream and determines what is then left of the
      /// stream. If 'pickhead' cannot identify an element and a stream rest
      /// the result is a ParseError.
      /// The second argument to SplitToParts is a value that indicates what
      /// an empty (processed) stream looks like.
      /// SplitToParts keeps applying 'pickhead' to collect elements and
      /// shorten the stream. Processing is complete once the stream rest is
      /// equal to the empty value. If a parse error occurs (in 'pickhead')
      /// then the entire expression is ParseError.
      static member SplitToParts (pickhead: 's -> ('a * 's) ParseResult) (emptyval: 's) (stream: 's) =
        let rec collectparts (acc: 'a list) (rest: 's) =
          if rest = emptyval then Result acc else
          match pickhead rest with
          | Result (part, tail) -> collectparts (part::acc) tail
          | ParseError e -> ParseError e
        let collectedparts = collectparts [] stream
        ParseResult.Map (collectedparts, fun partlist -> Result (List.rev partlist))
  
  
  /// The ParseResultExprBuilder class enables us to use the ParseResult class
  /// with computation expressions. Specifically we can use the "let!" keyword
  /// to unwrap a ParseResult (i.e. map "Result a" to plain "a").
  type public ParseResultExprBuilder =
    public new () = { }
    member public expr.Bind tuple = ParseResult.Map tuple
    member public expr.Return alpha = Result alpha
  
  
  /// Usually a parse result is returned as a ParseResult, either successfully
  /// as an 'a Result or as an ParseError in case of failure.
  /// At times, when a ParseResult is too much overhead, a parse result can be
  /// returned as an ordinary function result with failure indicated by
  /// raising a ParseErrorException.
  /// The ParseErrorException is particular useful for subroutines where the
  /// exception can be caught by the caller and turned into a ParseResult.
  exception ParseErrorException of string
  
