// Stormworks Vehicle Parser, a third party tool.
// Copyright (c) 2021, Robert Nielsen. All Rights Reserved.
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
  


// The three most important data types are ComponentInfoRec, GraphicsCube and
// a data type to keep track of the 'wet' and 'dry' parts of the vessel for
// buoyancy calculations.
// A list of ComponentInfoRec records represents the vessel structure
// components that make up the vessel. The vessel structure components are
// described in the Stormworks game vessel XML.
// A list of GraphicsCube records makes up the appearance of the vessel. The
// appearance can be exported and shown in Octave to help visualize the vessel
// and the calculation results.
// Remark: A data type to track wet and dry cubes or space has not been added
// yet!
namespace ACTConsult.Rodhern.StormworksVehicleParser.ComponentData
open ACTConsult.Rodhern.StormworksVehicleParser
  
  #nowarn "9" // File wide directive (see e.g. https://github.com/fsharp/fslang-suggestions/issues/278).
  open System.Runtime.InteropServices // Access to struct layout attributes for ColourValue.
  open System // Easy reference to System.Int32 by its more explicit name "Int32".
  
  
  /// Three dimensional single precision coordinate point.
  type Float32CoordValue =
       { XPos: single; YPos: single; ZPos: single }
       with
        /// Create Float32CoordValue from single precision coordinate tuple.
        static member public OfTriple (x, y, z) = { XPos= x; YPos= y; ZPos= z }
        
        // The only operation defined so far is simple addition.
        // See the IntCoordValue class for related operations.
        static member (+) (c1: Float32CoordValue, c2: Float32CoordValue) =
          { XPos = c1.XPos + c2.XPos; YPos = c1.YPos + c2.YPos; ZPos = c1.ZPos + c2.ZPos }
  
  
  /// An eight digit hexadecimal colour value in RGBA format.
  [< Struct; StructLayout(LayoutKind.Explicit, Size=4) >]
  type ColourValue =
       struct // Records cannot currently be [< Struct >].
         /// Create ColourValue from R, G, B and A byte values.
         public new (r, g, b, a) = { R= r; G= g; B= b; A= a }
         /// Red value 00 .. FF .
         [< FieldOffset(0) >]
         val public R: byte
         /// Green value 00 .. FF .
         [< FieldOffset(1) >]
         val public G: byte
         /// Blue value 00 .. FF .
         [< FieldOffset(2) >]
         val public B: byte
         /// Opacity (alpha) value 00 .. FF .
         [< FieldOffset(3) >]
         val public A: byte
       end
  
  
  /// The information used for an Octave patch.
  /// An Octave patch is a coloured polygon in three dimensional space.
  type GraphicsPolygon =
       {
         Corners: Float32CoordValue list
         Colour: ColourValue
       }
  
  
  /// A (usually short) list of polygons that make up the graphical
  /// representation of a 'cube' or 'component'.
  type GraphicsCube =
       {
         Faces: GraphicsPolygon list
       }
  
  
  /// Basic zero-indexed numbering of coordinate axes (0: X, 1: Y, 2: Z).
  type XYZIndex =
       | XIdx = 0
       | YIdx = 1
       | ZIdx = 2
  
  
  /// Three dimensional integer position coordinate point.
  type IntCoordValue =
       {
         XPosition: int
         YPosition: int
         ZPosition: int
       }
       with
        /// The string representation of the coordinates is on the form
        /// "(%d, %d, %d)"; e.g. the origin would be "(0, 0, 0)".
        override c.ToString () =
          sprintf "(%d, %d, %d)" c.XPosition c.YPosition c.ZPosition
        
        /// The default indexer (see c.Item below).
        member private c.GetItem (idx: XYZIndex) =
          match idx with
          | XYZIndex.XIdx -> c.XPosition
          | XYZIndex.YIdx -> c.YPosition
          | XYZIndex.ZIdx -> c.ZPosition
          | _ -> invalidArg "idx" "Valid index arguments are 'XIdx', 'YIdx' and 'ZIdx'."
        
        /// The default indexing is by coordinate axis number.
        /// That means the coordinates of an IntCoordValue c are
        /// c.[XYZIndex.XIdx], c.[XYZIndex.YIdx] and c.[XYZIndex.ZIdx].
        member public c.Item with get idx = c.GetItem idx
        
        /// Modify will set an individual coordinate value for a particular
        /// axis. The result is a modified copy of the original IntCoordValue.
        /// The coordinate values, XPosition, YPosition and ZPosition are all
        /// immutable, and so the default indexer is read-only, and you cannot
        /// do e.g. "c.[XYZIndex.XIdx] <- 5".
        member public c.Modify (idx: XYZIndex) (positionvalue: int) =
          match idx with
          | XYZIndex.XIdx -> { c with XPosition= positionvalue }
          | XYZIndex.YIdx -> { c with YPosition= positionvalue }
          | XYZIndex.ZIdx -> { c with ZPosition= positionvalue }
          | _ -> invalidArg "idx" "Valid index arguments are 'XIdx', 'YIdx' and 'ZIdx'."
        
        /// Create IntCoordValue from coordinate tuple.
        static member public OfTriple (x, y, z) = { XPosition= x; YPosition= y; ZPosition= z }
        
        /// Represents the coordinate system origin.
        static member public Zero with get () = IntCoordValue.OfTriple (0,0,0)
        
        // Some addition and scaling operations for IntCoordValue.
        // So far i+f is defined, for i: IntCoordValue and f: Float32CoordValue, but f+i is not.
        static member (+) (c1 : IntCoordValue, c2 : IntCoordValue) =
          { XPosition= c1.XPosition + c2.XPosition; YPosition= c1.YPosition + c2.YPosition; ZPosition= c1.ZPosition + c2.ZPosition }
        static member (+) (c1: IntCoordValue, c2: Float32CoordValue) =
          { XPos = c2.XPos + single c1.XPosition; YPos = c2.YPos + single c1.YPosition; ZPos = c2.ZPos + single c1.ZPosition }
        static member (*) (scale: int, c2: IntCoordValue) =
          { XPosition= scale * c2.XPosition; YPosition= scale * c2.YPosition; ZPosition= scale * c2.ZPosition }
        static member (*) (scalar: single, c2: IntCoordValue) =
          { XPos= scalar * (single c2.XPosition); YPos= scalar * (single c2.YPosition); ZPos= scalar * (single c2.ZPosition) }
  
  
  /// Helper type for CubeOrientation
  type AxisReference =
    val private xyzidx: XYZIndex
    val private axisinverted: bool
    
    /// Invert the direction of the axis.
    member public a.Invert () =
      new AxisReference (a.xyzidx, not a.axisinverted)
    
    /// The axis reference represented by an integer coordinate value.
    /// E.g. positive Z axis is represented by (0, 0, 1).
    member private a.GetAxisIntCoord () =
      let e = if a.axisinverted then -1 else +1
      match a.xyzidx with
      | XYZIndex.XIdx -> (e,0,0)
      | XYZIndex.YIdx -> (0,e,0)
      | XYZIndex.ZIdx -> (0,0,e)
      | _ -> invalidArg "xyzidx" "Internal error: Invalid indexing of coordinate axes."
      |> IntCoordValue.OfTriple

    /// The axis reference represented by an integer coordinate value,
    /// after multiplication with the given integer scale factor.
    /// E.g. positive Z axis scaled by 4 is equal to (0, 0, 4).
    member public a.ToDestinationPosition (scaleint: int) =
      let e = a.GetAxisIntCoord ()
      scaleint * e

    /// The axis reference scaled to the given magnitude.
    /// E.g. negative Y axis with magnitude 3.14 is equal to (0, -3.14, 0).
    member public a.ToDestinationPosition (magnitude: single) =
      let e = a.GetAxisIntCoord ()
      magnitude * e
    
    /// Simple private low level constructor.
    private new (idx, inv) =
      { xyzidx= idx; axisinverted= inv }
    
    /// Get the canonical reference representation of the X, Y or Z axis.
    public new (idx: XYZIndex) =
      AxisReference (idx, false)
    
    /// Identify an axis by its Stormworks unit column and flag properties.
    public new (column: Int32*Int32*Int32, srcinv: bool) =
      let isinverted (value: Int32) =
        match value with
        | -1 -> true
        | +1 -> false
        | _ -> raise (ParseErrorException "AxisReference constructor argument column entries must all be -1, 0 or +1.")
      let idx, colinv =
        match column with
        | x, 0, 0 -> XYZIndex.XIdx, isinverted x
        | 0, y, 0 -> XYZIndex.YIdx, isinverted y
        | 0, 0, z -> XYZIndex.ZIdx, isinverted z
        | _ -> raise (ParseErrorException "AxisReference constructor argument 'column' must have exactly one non-zero entry.")
// let tparinv = (int tparam) &&& (1 <<< (int idx)) > 0
// let inv = colinv <> tparinv // poor man's xor // Note: t is NOT used as post symmetry; the point of pre symmetry eludes me! (maybe it is not a geometry concern)
      let inv = colinv <> srcinv // poor man's xor
      { xyzidx= idx; axisinverted= inv }
  
  
  /// Orientation described by simple straight rotations and axis flips.
  type CubeOrientation =
    val xdst: AxisReference // tells where the 'x axis' ends up
    val ydst: AxisReference // tells where the 'y axis' ends up
    val zdst: AxisReference // tells where the 'z axis' ends up
    
    /// Rotate (and flip) the given coordinate according to the cube orientation.
    member public a.Orient (coords: Float32CoordValue) =
      let xcontrib = a.xdst.ToDestinationPosition coords.XPos
      let ycontrib = a.ydst.ToDestinationPosition coords.YPos
      let zcontrib = a.zdst.ToDestinationPosition coords.ZPos
      xcontrib + ycontrib + zcontrib
    
    /// The result is the origin corrected for relative offset,
    /// where the offset is rotated (and flipped) according to orientation.
    member public a.CalculateOffsetPosition (origin: IntCoordValue) (relativeoffset: IntCoordValue) =
      let xcontrib = a.xdst.ToDestinationPosition relativeoffset.XPosition
      let ycontrib = a.ydst.ToDestinationPosition relativeoffset.YPosition
      let zcontrib = a.zdst.ToDestinationPosition relativeoffset.ZPosition
      origin + xcontrib + ycontrib + zcontrib
    
    /// Helper method that brakes up a nine element integer list (an
    /// orientation matrix) to 3 columns of 3 integers each.
    /// The result is then presented as a list of 3 AxisReference values.
    static member private Columnwise (entries: Int32 list, tparam: int) =
      let rec take3 (srcidxs: XYZIndex list) (acc: AxisReference list) (es: Int32 list) =
        match srcidxs, es with
        | [], [] -> List.rev acc
        | aidx::idxstail, xsrc::ysrc::zsrc::tail
          -> let srcinv = (1 <<< (int aidx)) &&& (int tparam) > 0
             let aref = new AxisReference ((xsrc, ysrc, zsrc), srcinv)
             take3 idxstail (aref::acc) tail
        | _ -> raise (ParseErrorException "An orientation matrix should have 3 times 3 entries.")
      take3 [XYZIndex.XIdx; XYZIndex.YIdx; XYZIndex.ZIdx] [] entries
    
    /// Identify orientation by Stormworks orientation matrix
    /// and tparam flag property.
    public new (melements: Int32 list, tparam: int) =
      let xdst, ydst, zdst = // slightly roundabout, but the comiler is happier this way
        match CubeOrientation.Columnwise (melements, tparam) with
        | [xcol; ycol; zcol] -> xcol, ycol, zcol
        | _ -> raise (ParseErrorException "An orientation matrix should have 3 columns (each of 3 entries).")
      { xdst= xdst; ydst= ydst; zdst= zdst }
    
    /// The canonical orientation. Think of it as the identical map
    /// that neither rotates or mirrors the space.
    public new () =
      { xdst= new AxisReference (XYZIndex.XIdx); ydst= new AxisReference (XYZIndex.YIdx); zdst= new AxisReference (XYZIndex.ZIdx) }
  
