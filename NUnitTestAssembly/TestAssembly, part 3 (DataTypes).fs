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


open ACTConsult.Rodhern.StormworksVehicleParser.ComponentData

module GraphicsExportModule = // Might well at some point become part of the code proper.
  
  /// Write a transposed table of values to a (pure) text file.
  /// The table format has columns as the inner value,
  /// i.e. the table is a list of columns.
  let writeTableToFile (filepath: string) (table: single list list) =
    let culture = System.Globalization.CultureInfo.InvariantCulture;
    let nbcols = List.length table
    let nbrows = List.fold (fun curmax col -> max curmax (List.length col)) 0 table
    /// An array of rows with individual values formatted to string; entryij = entries.[ith row].[jth col] .
    let entries = Array.init nbrows (fun _ -> Array.create nbcols "NaN") // Warning: Octave wont fill the polygon if any NaN values are left!
    let iterfunc (i: int, j: int) (eij: single) = entries.[i].[j] <- eij.ToString (culture)
    do List.iteri (fun j colj -> List.iteri (fun i eij -> iterfunc (i,j) eij) colj) table
    let line (row: string []) =
      let sb = new System.Text.StringBuilder ()
      for j = 0 to nbcols - 1
       do if j > 0 then sb.Append "\t" |> ignore
          sb.Append row.[j] |> ignore
      sb.ToString ()
    let lines = entries |> Array.map line
    System.IO.File.WriteAllLines (filepath, lines)
  
  /// Turn a list of graphics cubes (GraphicsCube list) into
  /// a set of data tables that can be saved to files.
  let createTablesFromGraphicsCubeList (cubelist: GraphicsCube list): (string * single list list) list =
    let colourlist_idxval = new SortedList<int,ColourValue> ()
    let colourlist_validx = new SortedList<ColourValue,int> ()
    /// add the colour value to the list if it is not already in there
    let updatecolourlist (c: ColourValue) =
      if colourlist_validx.ContainsKey c then () else
      let newidx = colourlist_idxval.Count + 1
      colourlist_idxval.Add (newidx, c)
      colourlist_validx.Add (c, newidx)
    let allfaces = cubelist |> List.map (fun gc -> gc.Faces) |> List.concat
    let allcolours = allfaces |> List.map (fun face -> face.Colour)
    let allcorners = allfaces |> List.map (fun face -> face.Corners)
    do allfaces |> List.iter (fun face -> updatecolourlist face.Colour)
    let Xs = allcorners |> List.map (List.map (fun coord -> coord.XPos))
    let Ys = allcorners |> List.map (List.map (fun coord -> coord.YPos))
    let Zs = allcorners |> List.map (List.map (fun coord -> coord.ZPos))
    let Cidxs = allcolours |> List.map (fun colour -> [single colourlist_validx.[colour]]) // a row vector as matrix
    let Rcolumn = List.init colourlist_idxval.Count (fun i -> single colourlist_idxval.[i+1].R)
    let Gcolumn = List.init colourlist_idxval.Count (fun i -> single colourlist_idxval.[i+1].G)
    let Bcolumn = List.init colourlist_idxval.Count (fun i -> single colourlist_idxval.[i+1].B)
    [("X",Xs); ("Y",Ys); ("Z",Zs); ("Cidxs",Cidxs); ("Colours",[Rcolumn;Gcolumn;Bcolumn])]
  
  /// Full path for data file to use for the given table name.
  let getDataFolderFilePath (tablename: string) =
    raise (new System.NotImplementedException "Do not actually save the files when running the tests!")
    sprintf @"..\Data\Out\%s.txt" tablename
  
  /// Write files to hardcoded file location.
  let public writeCubeListToFiles (cubelist: GraphicsCube list) =
    let tables = createTablesFromGraphicsCubeList cubelist
    for name, table in tables
     do let filepath = getDataFolderFilePath name
        writeTableToFile filepath table


[< TestFixture; Category "DataTypes" >]
type GraphicsCubeTestClass () =
  
  let myGraphicsCube = GraphicsCubeTestClass.CreateStdGraphicsCube ()
  
  /// Creates roughly a 1x1x1 Rubik's cube.
  /// Note: To be reused below in CubeOrientationTestClass as well.
  static member CreateStdGraphicsCube () =
    let white  = new ColourValue (250uy, 250uy, 250uy, 150uy)
    let yellow = new ColourValue (250uy, 250uy,  50uy, 200uy)
    let orange = new ColourValue (250uy, 150uy,  50uy, 200uy)
    let red    = new ColourValue (250uy,  50uy,  50uy, 250uy)
    let green  = new ColourValue ( 50uy, 250uy,  50uy, 250uy)
    let blue   = new ColourValue ( 50uy,  50uy, 250uy, 250uy)
    let c: Float32CoordValue [,,] = // an array of corners to build a cube example (see below)
      let cornerfcv xarg yarg zarg =
        let posneg = function | 0 -> -0.5f | _ -> +0.5f
        (posneg xarg, posneg yarg, posneg zarg)
        |> Float32CoordValue.OfTriple
      Array3D.init 2 2 2 cornerfcv
    let whiteface  = { Corners= [ c.[0,1,1]; c.[1,1,1]; c.[1,1,0]; c.[0,1,0] ]; Colour = white }
    let yellowface = { Corners= [ c.[0,0,0]; c.[1,0,0]; c.[1,0,1]; c.[0,0,1] ]; Colour = yellow }
    let orangeface = { Corners= [ c.[0,1,1]; c.[0,1,0]; c.[0,0,0]; c.[0,0,1] ]; Colour = orange }
    let redface    = { Corners= [ c.[1,1,0]; c.[1,1,1]; c.[1,0,1]; c.[1,0,0] ]; Colour = red }
    let greenface  = { Corners= [ c.[0,1,0]; c.[1,1,0]; c.[1,0,0]; c.[0,0,0] ]; Colour = green }
    let blueface   = { Corners= [ c.[1,1,1]; c.[0,1,1]; c.[0,0,1]; c.[1,0,1] ]; Colour = blue }
    { Faces= [ whiteface; yellowface; orangeface; redface; greenface; blueface ] }
  
  /// Let us add up the corner coordinate values, because why not :-).
  /// Note: Used for 'CheckFloat32CoordValue' and to be reused below
  ///       in CubeOrientationTestClass as well.
  static member AddFaceCorners (poly: GraphicsPolygon) =
    let zero = 1.0f * IntCoordValue.Zero
    List.fold (+) zero poly.Corners
  
  /// Used to check a corner sum result. E.g. the yellow side is down,
  /// so that is Y direction negative, and the sum of four halves is two,
  /// all in all the yellow sum is (0,-2,0).
  /// Note: Used with 'AddFaceCorners' for 'CheckFloat32CoordValue'
  ///       and to be reused below in CubeOrientationTestClass as well.
  static member AssertFaceCornerSum (side: XYZIndex, negative: bool) (coord: Float32CoordValue) =
    let expval (axis: XYZIndex) =
      if side <> axis then 0.f elif negative then -2.f else 2.f
    Assert.AreEqual (expval XYZIndex.XIdx, coord.XPos, "'X' coordinate values should be (exactly) equal.")
    Assert.AreEqual (expval XYZIndex.YIdx, coord.YPos, "'Y' coordinate values should be (exactly) equal.")
    Assert.AreEqual (expval XYZIndex.ZIdx, coord.ZPos, "'Z' coordinate values should be (exactly) equal.")
  
  [< Test >]
  member test.CheckColourValue (): unit =
    let colourOfOrangeFace = myGraphicsCube.Faces.Tail.Tail.Head.Colour
    Assert.AreEqual (250uy, colourOfOrangeFace.R, "The orange face is expected to have almost full red colour intensity.")
    Assert.AreEqual (150uy, colourOfOrangeFace.G, "The orange face is expected to have medium level green colour intensity.")
    Assert.AreEqual ( 50uy, colourOfOrangeFace.B, "The orange face is expected to have almost no blue colour intensity.")
    Assert.AreEqual (200uy, colourOfOrangeFace.A, "The orange face is expected to have high opacity.")
  
  [< Test >]
  member test.TestFloat32CoordValueSimpleAddition (): unit =
    let X1 = Float32CoordValue.OfTriple ( 11.55f,  22.55f,  33.55f)
    let X2 = Float32CoordValue.OfTriple (-10.45f, -20.35f, -30.25f)
    let S = X1 + X2
    Assert.AreEqual (1.1, double S.XPos, 1E-4, "Sum of two coordinate values failed in 'x' coordinate.")
    Assert.AreEqual (2.2, double S.YPos, 1E-4, "Sum of two coordinate values failed in 'y' coordinate.")
    Assert.AreEqual (3.3, double S.ZPos, 1E-4, "Sum of two coordinate values failed in 'z' coordinate.")
  
  [< Test >]
  member test.CheckFloat32CoordValue (): unit =
    let addcorners = GraphicsCubeTestClass.AddFaceCorners
    let checkresult = GraphicsCubeTestClass.AssertFaceCornerSum
    let expectedsums = // i.e white, yellow, orange, red, green and blue sides
      [ XYZIndex.YIdx, false; XYZIndex.YIdx, true; XYZIndex.XIdx, true;
        XYZIndex.XIdx, false; XYZIndex.ZIdx, true; XYZIndex.ZIdx, false ]
    let actualsums = List.map addcorners myGraphicsCube.Faces
    List.iter2 checkresult expectedsums actualsums 
  
  [< Test >]
  member test.NotImplementedExportExample (): unit =
    /// An example that (except for a deliberately thrown exception in
    /// getDataFolderFilePath) will save the 1x1x1 'myGraphicsCube' example
    /// scene to Octave readable files.
    let writeToFilesExample () =
      GraphicsExportModule.writeCubeListToFiles [ myGraphicsCube ]
    let message = "The file save example is supposed to be interrupted by a NotImplementedException."
    Assert.Throws (typeof<System.NotImplementedException>, writeToFilesExample, message) |> ignore


[< TestFixture; Category "DataTypes" >]
type CubeOrientationTestClass () =
  
  let x112233 = IntCoordValue.OfTriple (11, 22, 33)
  let myGraphicsCube = GraphicsCubeTestClass.CreateStdGraphicsCube ()
  
  let rotateGrahicsCube (orientation: CubeOrientation) (cube: GraphicsCube) =
    let rotateCorner = orientation.Orient
    let rotateFace (gp: GraphicsPolygon) =
      { Colour= gp.Colour; Corners= List.map rotateCorner gp.Corners }
    { Faces= List.map rotateFace cube.Faces }
  
  [< Test >]
  member test.CheckIntCoordValueZero (): unit =
    let x = x112233
    Assert.AreEqual(x, IntCoordValue.Zero + x, "Adding zero should make no difference.")
    Assert.AreEqual(x, x + IntCoordValue.Zero, "Adding zero should make no difference.")
  
  [< Test >]
  member test.IntCoordValueScalingTest (): unit =
    let x1 = IntCoordValue.OfTriple (10, 20, 30)
    let x2 = IntCoordValue.OfTriple ( 4, 14, 15)
    let y = Float32CoordValue.OfTriple (18.f, 48.f, 60.f) // y = x1 + 2*x2
    Assert.AreEqual (y, 1.f*(1*x1 + 2*x2), "Multiply and add using ints.")
    Assert.AreEqual (y, 1.f*x1 + 1.f*(2*x2), "Multiply using ints and add using floats.")
    Assert.AreEqual (y, 1.f*x1 + 2.f*x2, "Multiply and add using floats.")
    Assert.AreEqual (y, x1 + 2.f*x2, "Use implicit unit scaling when adding floats to ints.")
  
  [< Test >]
  member test.CheckIntCoordValueToString (): unit =
    Assert.AreEqual ("(11, 22, 33)", x112233.ToString (), "Check output string value.")
  
  [< Test >]
  member test.CheckIntCoordValueIndexer (): unit =
    Assert.AreEqual(11, x112233.[XYZIndex.XIdx], "First coordinate component is X=11.")
    Assert.AreEqual(22, x112233.[XYZIndex.YIdx], "First coordinate component is Y=22.")
    Assert.AreEqual(33, x112233.[XYZIndex.ZIdx], "First coordinate component is Z=33.")
  
  [< Test >]
  member test.IntCoordValueModifyTest (): unit =
    raise (new System.NotImplementedException "Test case not implemented yet!")
  
  [< Test >]
  member test.CheckAxisReferenceInvert (): unit =
    raise (new System.NotImplementedException "Test case not implemented yet!")
  
  [< Test >]
  member test.CheckAxisReferenceDestinationPosition (): unit = // two variations (scaleint: int), (magnitude: single)
    raise (new System.NotImplementedException "Test case not implemented yet!")
  
  [< Test >]
  member test.CheckAxisReferenceConstructorByIdx (): unit =
    raise (new System.NotImplementedException "Test case not implemented yet!")
  
  [< Test >]
  member test.AxisReferenceConstructorColumnMatchTest (): unit =
    raise (new System.NotImplementedException "Test case not implemented yet!")
  
  [< Test >]
  member test.CheckCubeOrientationIdMap (): unit = // Check the default 1:1 map of cube
    let idRotation = new CubeOrientation ()
    let rotatedCube = rotateGrahicsCube idRotation myGraphicsCube
    Assert.AreEqual (myGraphicsCube, rotatedCube, "The id-map rotated cube should be identical to the untouched cube.")
  
  [< Test >]
  member test.TestCubeOrientationRotation (): unit = // Check a rotation without mirroring of the cube
    let addcorners = GraphicsCubeTestClass.AddFaceCorners
    let checkresult = GraphicsCubeTestClass.AssertFaceCornerSum
    let rotation = new CubeOrientation ([ 0; 1; 0; 0; 0; 1; 1; 0; 0 ], 0)
    let rotatedCube = rotateGrahicsCube rotation myGraphicsCube
    let expectedsums = // i.e white, yellow, orange, red, green and blue sides after rotation
      [ XYZIndex.ZIdx, false; XYZIndex.ZIdx, true; XYZIndex.YIdx, true;
        XYZIndex.YIdx, false; XYZIndex.XIdx, true; XYZIndex.XIdx, false ]
    let actualsums = List.map addcorners rotatedCube.Faces
    List.iter2 checkresult expectedsums actualsums 
  
  [< Test >]
  member test.TestCubeOrientationRelativeMap (): unit = // Check a relative int mapping
    raise (new System.NotImplementedException "Test case not implemented yet!")
  
  [< Test >]
  member test.TestCubeOrientation_tparam (): unit = // Check some tparam influence
    raise (new System.NotImplementedException "Test case not implemented yet!")
