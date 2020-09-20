// Stormworks Vehicle Parser, a third party tool.
// Copyright (c) 2020, Robert Nielsen. All Rights Reserved.
// See License.txt in the repository root for license information.
namespace ACTConsult.Rodhern.StormworksVehicleParser
  
  
  /// A key-value source must do two things;
  /// it must provide the complete set of keys in use, and for every
  /// given key it must be able to retrieve all corresponding values.
  /// A key-value source is used to build a KeyedCollection.
  type IKeyValueSource<'a,'k when 'k: comparison> =
    abstract member GetKeys: unit -> 'k Set
    abstract member GetValues: string -> 'a list
  
  
  /// The keyed collection provides access to a key-value source, where
  /// the key-value source is an immutable collection of data items each
  /// associated with a string-valued key.
  /// The keyed collection keeps track of which keys' data items have been
  /// fetched/retrieved (or actively ignored). The keyed collection guards
  /// against fetching the same key twice, and allows the user to verify that
  /// no keys are left 'untouched'/forgotten.
  type 'a KeyedCollection (source: IKeyValueSource<'a,string>) =
    let mutable remainingKeys = source.GetKeys ()
    
    /// The keys may or may not be in the collection.
    /// In any case the keys are 'meant' to be ignored.
    /// Returns the number of ignored keys for information.
    member public kc.Ignore (keylist: string list) =
      let keycountbefore = remainingKeys.Count
      do remainingKeys <- Set.difference remainingKeys (Set.ofList keylist)
      keycountbefore - remainingKeys.Count
    
    /// Retrieve well defined single value for given key.
    member public kc.Get (key: string): 'a ParseResult =
      if not (remainingKeys.Contains key)
       then ParseError (sprintf "Key \"%s\" is not available in keyed collection." key)
       else
      remainingKeys <- Set.remove key remainingKeys
      match source.GetValues key with
      | [v] -> Result v
      | _ -> ParseError (sprintf "Key \"%s\" returned multiple (or zero) associated values." key)
    
    /// Fetch associated values (zero or more) for given key.
    member public kc.GetOptionalSingleOrMultiple (key: string): ('a list) ParseResult =
      if not (remainingKeys.Contains key)
       then Result []
       else remainingKeys <- Set.remove key remainingKeys
            Result (source.GetValues key)
    
    /// Return a ParseError if some keys are still not removed
    /// (i.e. neither ignored or retrieved).
    member public kc.VerifyNoKeysLeft (): unit ParseResult =
      if remainingKeys.IsEmpty
       then Result ()
       else ParseError (sprintf "There was/were %d remaining unused key(s); ranging from \"%s\" to \"%s\"." remainingKeys.Count remainingKeys.MinimumElement remainingKeys.MaximumElement)
    
    /// Some collections are so full that instead of aiming for emptying them
    /// we just count up the remaining keys.
    member public kc.NumberOfKeysLeft () =
      remainingKeys.Count
  
