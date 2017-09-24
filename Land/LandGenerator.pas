unit LandGenerator;

interface

  uses
    Land, Collection, Windows, Graphics;

  const
    LandMax = 10000;

  type
    PLandMatrix = ^TLandMatrix;
    TLandMatrix = array[0..LandMax*LandMax] of TLandVisualClassId;

    TLandCheckOption  = (chkCenter, chkBorder, chkDontCare);
    TLandDetectMatrix = array[0..2, 0..2] of TLandCheckOption;

    TVisualClass =
      class
        public
          constructor Create( aLandVisualClassId : TLandVisualClassId; aMapColor : TColor );
        private
          fLandVisualClassId : TLandVisualClassId;
          fMapColor          : TColor;
        public
          property LandVisualClassId : TLandVisualClassId read fLandVisualClassId;
          property MapColor          : TColor             read fMapColor;
      end;

    TLandDetector =
      class
        public
          constructor Create( aCenterClass, aBorderClass : TLandClass; aLandType : TLandType );
        private
          fCenterClass     : TLandClass;
          fBorderClass     : TLandClass;
          fLandType        : TLandType;
          fDetectionMatrix : TLandDetectMatrix;
          fVisualClasses   : TCollection;
        public
          function  DetectLand( x, y : integer; LandSource : PLandMatrix; LandWidth : integer ) : TLandVisualClassId;
          function  RandomLand : TLandVisualClassId;
          function  VisualClassMatches( aLandClass : TLandClass; aLandType : TLandType ) : boolean;
          procedure AddVisualClass( aVisualClass : TVisualClass );
        protected
          function GenerateLandDetectMatrix : TLandDetectMatrix; virtual;
      end;

    TLandRenderer =
      class
        public
          constructor Create( ClassDir : string );
          destructor  Destroy; override;
        private
          fLandSize   : TPoint;
          fLandBitmap : TBitmap;
          fLandSource : PLandMatrix;
          fLandDest   : PLandMatrix;
          fDetectors  : TCollection;
          fSpecials   : array[TLandClass] of TCollection;
          fClassCount : integer;
        public
          fMainColors : array[TLandClass] of TColor;
        public
          property LandSize   : TPoint      read fLandSize;
          property LandSource : PLandMatrix read fLandSource;
          property LandDest   : PLandMatrix read fLandDest;
          property ClassCount : integer     read fClassCount;
        private
          fVisualClasses : TCollection;
        private
          function GetVisualClass( VisualClassId : TLandVisualClassId ) : TVisualClass;
        public
          property VisualClass[VisualClassId : TLandVisualClassId] : TVisualClass read GetVisualClass;
          property VisualClasses : TCollection read fVisualClasses;
        public
          procedure RenderLand;
          procedure LoadLandInfo( filename : string );
          procedure LoadRenderedLand( filename : string );
        private
          fColors : array[TLandClass] of TColor;
        private
          procedure ScanForColors;
          function  GetDetector( aLandClass : TLandClass; aLandType : TLandType ) : TLandDetector;
      end;

implementation

  uses
    SysUtils, IniFiles;

  const
    O = chkCenter;
    m = chkBorder;
    x = chkDontCare;

  const
    CommonDetectors :
      array[TLandType] of TLandDetectMatrix =
        // Center
        ( ((O, O, O),
           (O, O, O),
           (O, O, O)),
        // N
          ((x, m, x),
           (O, O, O),
           (x, x, x)),
        // E
          ((x, O, x),
           (x, O, m),
           (x, O, x)),
        // S
          ((x, x, x),
           (O, O, O),
           (x, m, x)),
        // W
          ((x, O, x),
           (m, O, x),
           (x, O, x)),
        // NEo
          ((x, m, x),
           (O, O, m),
           (x, O, x)),
        // SEo
          ((x, O, x),
           (O, O, m),
           (x, m, x)),
        // SWo
          ((x, O, x),
           (m, O, O),
           (x, m, x)),
        // NWo
          ((x, m, x),
           (m, O, O),
           (x, O, x)),
        // NEi
          ((x, O, m),
           (x, O, O),
           (x, x, x)),
        // SEi
          ((x, x, x),
           (x, O, O),
           (x, O, m)),
        // SWi
          ((x, x, x),
           (O, O, x),
           (m, O, x)),
        // NWi
          ((m, O, x),
           (O, O, x),
           (x, x, x)),
        // Special
          ((O, O, O),
           (O, O, O),
           (O, O, O)) );


  // TVisualClass

  constructor TVisualClass.Create( aLandVisualClassId : TLandVisualClassId; aMapColor : TColor );
    begin
      inherited Create;
      fLandVisualClassId := aLandVisualClassId;
      fMapColor          := aMapColor;
    end;


  // TLandDetector

  constructor TLandDetector.Create( aCenterClass, aBorderClass : TLandClass; aLandType : TLandType );
    begin
      inherited Create;
      fCenterClass     := aCenterClass;
      fBorderClass     := aBorderClass;
      fLandType        := aLandType;
      fVisualClasses   := TCollection.Create( 0, rkUse );
      fDetectionMatrix := GenerateLandDetectMatrix;
    end;

  function TLandDetector.DetectLand( x, y : integer; LandSource : PLandMatrix; LandWidth : integer ) : TLandVisualClassId;
    var
      matches : boolean;
      i, j    : integer;
      Check   : TLandCheckOption;
      LandId  : TLandVisualClassId;
    begin
      if fVisualClasses.Count > 0
        then
          begin
            matches := true;
            i := 0;
            while (i < 3) and matches do
              begin
                j := 0;
                while (j < 3) and matches do
                  begin
                    Check  := fDetectionMatrix[i, j];
                    LandId := LandSource[LandWidth*(y - 1 + i) + x - 1 + j];
                    matches :=
                      (LandId = NoLand) or
                      (Check = chkDontCare) or
                      (Check = chkCenter) and (Land.LandClassOf( LandId ) = fCenterClass) or
                      (Check = chkBorder) and (Land.LandClassOf( LandId ) = fBorderClass);
                    inc( j );
                  end;
                inc( i );
              end;
            if matches
              then result := RandomLand
              else result := NoLand;
          end
        else result := NoLand;
    end;

  function TLandDetector.RandomLand : TLandVisualClassId;
    begin
      result := TVisualClass(fVisualClasses[random(fVisualClasses.Count)]).LandVisualClassId;
    end;

  function TLandDetector.VisualClassMatches( aLandClass : TLandClass; aLandType : TLandType ) : boolean;
    begin
      result := (fCenterClass = aLandClass) and (fLandType = aLandType);
    end;

  procedure TLandDetector.AddVisualClass( aVisualClass : TVisualClass );
    begin
      fVisualClasses.Insert( aVisualClass );
    end;

  function TLandDetector.GenerateLandDetectMatrix : TLandDetectMatrix;
    begin
      result := CommonDetectors[fLandType];
    end;


  // TLandRenderer

  constructor TLandRenderer.Create( ClassDir : string );

    procedure LoadClasses;
      var
        SearchRec   : TSearchRec;
        found       : boolean;
        IniFile     : TIniFile;
        Id          : TLandVisualClassId;
        Color       : TColor;
        VisualClass : TVisualClass;
        LandClass   : TLandClass;
        LandType    : TLandType;
        Detector    : TLandDetector;
      begin
        found := FindFirst( ClassDir + '*.ini', faAnyFile, SearchRec ) = 0;
        while found do
          begin
            IniFile     := TIniFile.Create( ClassDir + SearchRec.Name );
            Id          := IniFile.ReadInteger( 'General', 'Id', NoLand );
            Color       := IniFile.ReadInteger( 'General', 'MapColor', 0 );
            VisualClass := TVisualClass.Create( Id, Color );
            LandClass   := Land.LandClassOf( VisualClass.LandVisualClassId );
            LandType    := Land.LandTypeOf( VisualClass.LandVisualClassId );
            if LandType <> ldtSpecial
              then
                begin
                  if LandIsPure( Id )
                    then fMainColors[LandClassOf( Id )] := Color;
                  Detector := GetDetector( LandClass, LandType );
                  if Detector = nil
                    then
                      begin
                        Detector := TLandDetector.Create( LandClass, Land.LandBorders[LandClass], LandType );
                        fDetectors.Insert( Detector );
                      end;
                  Detector.AddVisualClass( VisualClass );
                end
              else fSpecials[LandClass].Insert( pointer(Id) );
            fVisualClasses.Insert( VisualClass );
            found := FindNext( SearchRec ) = 0;
            inc( fClassCount );
          end;
      end;

    var
      landClass : TLandClass;
    begin
      inherited Create;
      fDetectors := TCollection.Create( 0, rkBelonguer );
      fVisualClasses := TCollection.Create( 0, rkBelonguer );
      for landClass := low(landClass) to high(landClass) do
        fSpecials[landClass] := TCollection.Create( 0, rkUse );
      LoadClasses;
    end;

  destructor TLandRenderer.Destroy;
    var
      landClass : TLandClass;
    begin
      fDetectors.Free;
      fLandBitmap.Free;
      fVisualClasses.Free;
      for landClass := low(landClass) to high(landClass) do
        fSpecials[landClass].Free;
      inherited;
    end;

  function TLandRenderer.GetVisualClass( VisualClassId : TLandVisualClassId ) : TVisualClass;
    var
      i : integer;
    begin
      i := 0;
      while (i < fVisualClasses.Count) and (TVisualClass(fVisualClasses[i]).LandVisualClassId <> VisualClassId) do
        inc( i );
      if i < fVisualClasses.Count
        then result := TVisualClass(fVisualClasses[i])
        else result := nil;
    end;

  procedure TLandRenderer.LoadLandInfo( filename : string );

    function LandValueOfColor( Color : TColor ) : TLandVisualClassId;
      var
        LandClass : TLandClass;
      begin
        LandClass := low(LandClass);
        while (LandClass <= high(LandClass)) and (fColors[LandClass] <> Color) do
          inc( LandClass );
        result := Land.LandClassIds[LandClass];
      end;

    var
      x, y : integer;
    begin
      fLandBitmap := TBitmap.Create;
      fLandBitmap.LoadFromFile( filename );
      ScanForColors;
      fLandSize.x := fLandBitmap.Width;
      fLandSize.y := fLandBitmap.Height;
      getmem( fLandSource, fLandSize.y*fLandSize.x*sizeof(fLandSource[0]) );
      fillchar( fLandSource^, fLandSize.y*fLandSize.x*sizeof(fLandSource[0]), NoLand );
      getmem( fLandDest, fLandSize.y*fLandSize.x*sizeof(fLandSource[0]) );
      fillchar( fLandDest^, fLandSize.y*fLandSize.x*sizeof(fLandSource[0]), NoLand );
      for y := 0 to pred(fLandSize.y) do
        for x := 0 to pred(fLandSize.x) do
          fLandSource[fLandSize.x*y + x] := LandValueOfColor( fLandBitmap.Canvas.Pixels[x, y] );
    end;

  procedure TLandRenderer.LoadRenderedLand( filename : string );
    var
      x, y : integer;
      line : PByteArray;
    begin
      fLandBitmap := TBitmap.Create;
      fLandBitmap.LoadFromFile( filename );
      fLandSize.x := fLandBitmap.Width;
      fLandSize.y := fLandBitmap.Height;
      getmem( fLandSource, fLandSize.y*fLandSize.x*sizeof(fLandSource[0]) );
      fillchar( fLandSource^, fLandSize.y*fLandSize.x*sizeof(fLandSource[0]), NoLand );
      {
      getmem( fLandDest, fLandSize.y*fLandSize.x*sizeof(fLandSource[0]) );
      fillchar( fLandDest^, fLandSize.y*fLandSize.x*sizeof(fLandSource[0]), NoLand );
      }
      for y := 0 to pred(fLandSize.y) do
        begin
          line := fLandBitmap.ScanLine[y];
          for x := 0 to pred(fLandSize.x) do
            fLandSource[fLandSize.x*y + x] := line[x];
        end;
      fLandBitmap.Free;
    end;

  procedure TLandRenderer.RenderLand;
    const
      SpecialProb : array[TLandClass] of integer = (40, 30, 20, 0);
    var
      x, y, i    : integer;
      AccurateId : TLandVisualClassId;
      Center     : TLandDetector;
      LandClass  : TLandClass;
    begin
      for y := 1 to fLandSize.y - 2 do
        for x := 1 to fLandSize.x - 2 do
          begin
            i := 0;
            repeat
              AccurateId := TLandDetector(fDetectors[i]).DetectLand( x, y, fLandSource, fLandSize.x );
              inc( i );
            until (i = fDetectors.Count) or (AccurateId <> NoLand);
            if AccurateId = NoLand
              then
                begin
                  Center := GetDetector( LandClassOf(fLandSource[y*fLandSize.x + x]), ldtCenter );
                  AccurateId := Center.RandomLand;
                end;
            LandClass := LandClassOf( AccurateId );
            if LandIsPure( AccurateId ) and (random(100) < SpecialProb[LandClass])
              then AccurateId := byte(fSpecials[LandClass][random(fSpecials[LandClass].Count)]);
            fLandDest[fLandSize.x*y + x] := AccurateId;
          end;
    end;

  procedure TLandRenderer.ScanForColors;
    var
      i : TLandClass;
    begin
      for i := low(i) to high(i) do
        fColors[i] := fLandBitmap.Canvas.Pixels[integer(i), 0];
    end;

  function TLandRenderer.GetDetector( aLandClass : TLandClass; aLandType : TLandType ) : TLandDetector;
    var
      i : integer;
    begin
      i := 0;
      while (i < fDetectors.Count) and not (TLandDetector(fDetectors[i]).VisualClassMatches( aLandClass, aLandType )) do
        inc( i );
      if i < fDetectors.Count
        then result := TLandDetector(fDetectors[i])
        else result := nil;
    end;



end.



