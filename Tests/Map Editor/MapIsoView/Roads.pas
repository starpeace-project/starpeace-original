unit Roads;

interface

  uses
    Circuits, VoyagerServerInterfaces, MapTypes, GameTypes, Land;

  type
    TRoadBlockId =
      (
        rbNone,
        rbNSRoadStart,
        rbNSRoadEnd,
        rbWERoadStart,
        rbWERoadEnd,
        rbNSRoad,
        rbWERoad,
        rbLeftPlug,
        rbRightPlug,
        rbTopPlug,
        rbBottomPlug,
        rbCornerW,
        rbCornerS,
        rbCornerN,
        rbCornerE,
        rbCrossRoads
      );

  type
    TRoadBlockIdSet = set of TRoadBlockId;

  const
    NorthPointingBlocks =
      [
        rbNSRoadEnd,
        rbNSRoad,
        rbLeftPlug,
        rbRightPlug,
        rbTopPlug,
        rbCornerS,
        rbCornerE,
        rbCrossRoads
      ];

    SouthPointingBlocks =
      [
        rbNSRoadStart,
        rbNSRoad,
        rbLeftPlug,
        rbRightPlug,
        rbBottomPlug,
        rbCornerW,
        rbCornerN,
        rbCrossRoads
      ];

    EastPointingBlocks =
      [
        rbWERoadStart,
        rbWERoad,
        rbRightPlug,
        rbTopPlug,
        rbBottomPlug,
        rbCornerW,
        rbCornerS,
        rbCrossRoads
      ];

    WestPointingBlocks =
      [
        rbWERoadEnd,
        rbWERoad,
        rbLeftPlug,
        rbTopPlug,
        rbBottomPlug,
        rbCornerN,
        rbCornerE,
        rbCrossRoads
      ];

  type
    TRoadIdsMap = array [0..0] of TRoadBlockId;
    PRoadIdsMap = ^TRoadIdsMap;

  type
    IRoadsRendering =
      interface(ICircuitsRendering)
        function  GetRoadId(Row, Col : integer) : TRoadBlockId;
        procedure SetRoadId(Row, Col : integer; Value : TRoadBlockId);
        property RoadIds[Row, Col : integer] : TRoadBlockId read GetRoadId write SetRoadId; default;
      end;

  function  RenderRoadSegments(const SegmentsReport : TSegmentReport; Left, Top, Width, Height : integer) : ICircuitsRendering;
  procedure RenderRoadSegment(const Rendering : IRoadsRendering; const Segment : TSegmentInfo);

  const
    cRoadTopIdMask  = $0F;
    cHighRoadIdMask = $F0;

  const
    cLandTypeShift = 4;

  const
    cLandRoad        = 0;
    cUrbanRoad       = 1;
    cNorthBridge     = 2;
    cSouthBridge     = 3;
    cEastBridge      = 4;
    cWestBridge      = 5;
    cFullBridge      = 6;
    cLevelPass       = 7;
    cUrbanLevelPass  = 8;
    cSmoothRoad      = 9;
    cUrbanSmoothRoad = 10;
    cDummyRoadMask   = $100;

  type
    TRoad = word;

  const
    roadNone = high(TRoad);

  function RoadIdOf(roadblock : TRoad) : TRoadBlockId;
  function HighRoadIdOf(roadblock : idRoadBlock) : byte;
  function MakeRoadBlockOf(topid : TRoadBlockId; highid : byte) : idRoadBlock;
  function RoadBlockId(topolid : TRoadBlockId; landid : idLand; onconcrete, onrailroad, isdummy : boolean) : TRoad;
  function IsBridge(roadblock : TRoad) : boolean;
  function RotateRoadBlockId(id : integer; rotation : TRotation) : integer;

implementation

  type
    TRoadsRendering =
      class(TInterfacedObject, IRoadsRendering)
        private
          fRoadIds : PRoadIdsMap;
          fTop        : integer;
          fLeft       : integer;
          fWidth      : integer;
          fHeight     : integer;
        private
          function  IsValidAddress(Row, Col : integer) : boolean;
        private // IRoadsRendering
          function  GetRoadId(Row, Col : integer) : TRoadBlockId;
          procedure SetRoadId(Row, Col : integer; Value : TRoadBlockId);
        public
          constructor Create(Top, Left, Width, Height : integer);
          destructor  Destroy; override;
      end;

  // TRoadsRendering

  constructor TRoadsRendering.Create(Top, Left, Width, Height : integer);
    begin
      inherited Create;
      fTop := Top;
      fLeft := Left;
      fWidth := Width;
      fHeight := Height;
      getmem(fRoadIds, fWidth*fHeight*sizeof(TRoadBlockId));
      fillchar(fRoadIds^, fWidth*fHeight*sizeof(TRoadBlockId), ord(rbNone));
    end;

  destructor TRoadsRendering.Destroy;
    begin
      freemem(fRoadIds);
      inherited;
    end;

  function TRoadsRendering.IsValidAddress(Row, Col : integer) : boolean;
    begin
      Result := (Row >= fTop) and (Row < fTop + fHeight) and (Col >= fLeft) and (Col < fLeft + fWidth);
    end;

  function TRoadsRendering.GetRoadId(Row, Col : integer) : TRoadBlockId;
    begin
      if IsValidAddress(Row, Col)
        then Result := fRoadIds[(Row - fTop)*fWidth + Col - fLeft]
        else Result := rbNone;
    end;

  procedure TRoadsRendering.SetRoadId(Row, Col : integer; Value : TRoadBlockId);
    begin
      if IsValidAddress(Row, Col)
        then fRoadIds[(Row - fTop)*fWidth + Col - fLeft] := Value;
    end;

  procedure RenderRoadSegment(const Rendering : IRoadsRendering; const Segment : TSegmentInfo);
    var
      x    : integer;
      xmin : integer;
      xmax : integer;
      y    : integer;
      ymin : integer;
      ymax : integer;

    procedure RenderNSRoadStart;
      const
        NSRoadStartMappings : array [TRoadBlockId] of TRoadBlockId =
          (
            rbNSRoadStart, rbNSRoadStart, rbNSRoad,     rbCornerW,
            rbCornerN,     rbNSRoad,      rbBottomPlug, rbLeftPlug,
            rbRightPlug,   rbCrossRoads,  rbBottomPlug, rbCornerW,
            rbRightPlug,   rbCornerN,     rbLeftPlug,   rbCrossRoads
          );
      begin
        Rendering[y, x] := NSRoadStartMappings[Rendering[y, x]];
      end;

    procedure RenderNSRoadBlock;
      const
        NSRoadBlockMappings : array [TRoadBlockId] of TRoadBlockId =
          (
            rbNSRoad,    rbNSRoad,     rbNSRoad,     rbRightPlug,
            rbLeftPlug,  rbNSRoad,     rbCrossRoads, rbLeftPlug,
            rbRightPlug, rbCrossRoads, rbCrossRoads, rbRightPlug,
            rbRightPlug, rbLeftPlug,   rbLeftPlug,   rbCrossRoads
          );
      begin
        Rendering[y, x] := NSRoadBlockMappings[Rendering[y, x]];
      end;

    procedure RenderNSRoadEnd;
      const
        NSRoadEndMappings : array [TRoadBlockId] of TRoadBlockId =
          (
            rbNSRoadEnd, rbNSRoad,   rbNSRoadEnd,  rbCornerS,
            rbCornerE,   rbNSRoad,   rbTopPlug,    rbLeftPlug,
            rbRightPlug, rbTopPlug,  rbCrossRoads, rbRightPlug,
            rbCornerS,   rbLeftPlug, rbCornerE,    rbCrossRoads
          );
      begin
        Rendering[y, x] := NSRoadEndMappings[Rendering[y, x]];
      end;

    procedure RenderWERoadStart;
      const
        WERoadStartMappings : array [TRoadBlockId] of TRoadBlockId =
          (
            rbWERoadStart, rbCornerW,    rbCornerS,    rbWERoadStart,
            rbWERoad,      rbRightPlug,  rbWERoad,     rbCrossRoads,
            rbRightPlug,   rbTopPlug,    rbBottomPlug, rbCornerW,
            rbCornerS,     rbBottomPlug, rbTopPlug,    rbCrossRoads
          );
      begin
        Rendering[y, x] := WERoadStartMappings[Rendering[y, x]];
      end;

    procedure RenderWERoadBlock;
      const
        WERoadBlockMappings : array [TRoadBlockId] of TRoadBlockId =
          (
            rbWERoad,     rbBottomPlug, rbTopPlug,    rbWERoad,
            rbWERoad,     rbCrossRoads, rbWERoad,     rbCrossRoads,
            rbCrossRoads, rbTopPlug,    rbBottomPlug, rbBottomPlug,
            rbTopPlug,    rbBottomPlug, rbTopPlug,    rbCrossRoads
          );
      begin
        Rendering[y, x] := WERoadBlockMappings[Rendering[y, x]];
      end;

    procedure RenderWERoadEnd;
      const
        WERoadEndMappings : array [TRoadBlockId] of TRoadBlockId =
          (
            rbWERoadEnd,  rbCornerN,  rbCornerE,    rbWERoad,
            rbWERoadEnd,  rbLeftPlug, rbWERoad,     rbLeftPlug,
            rbCrossRoads, rbTopPlug,  rbBottomPlug, rbBottomPlug,
            rbTopPlug,    rbCornerN,  rbCornerE,    rbCrossRoads
          );
      begin
        Rendering[y, x] := WERoadEndMappings[Rendering[y, x]];
      end;

    begin
      with Segment do
        if x1 = x2
          then
            begin
              x := x1;
              ymin := y1;
              if ymin > y2
                then
                  begin
                    ymin := y2;
                    ymax := y1;
                  end
                else ymax := y2;
              y := ymin;
              RenderNSRoadEnd;
              inc( y );
              while y < ymax do
                begin
                  RenderNSRoadBlock;
                  inc( y );
                end;
              if y = ymax
                then RenderNSRoadStart;
            end
          else
            if y1 = y2
              then
                begin
                  y := y1;
                  xmin := x1;
                  if xmin > x2
                    then
                      begin
                        xmin := x2;
                        xmax := x1;
                      end
                    else xmax := x2;
                  x := xmin;
                  RenderWERoadStart;
                  inc( x );
                  while x < xmax do
                    begin
                      RenderWERoadBlock;
                      inc( x );
                    end;
                  if x = xmax
                    then RenderWERoadEnd;
                end;
    end;

  function RenderRoadSegments(const SegmentsReport : TSegmentReport; Left, Top, Width, Height : integer) : ICircuitsRendering;
    var
      SegmentIdx   : integer;
      CurRendering : IRoadsRendering;
    begin
      CurRendering := TRoadsRendering.Create(Top, Left, Width, Height);
      for SegmentIdx := 0 to pred(SegmentsReport.SegmentCount) do
        RenderRoadSegment(CurRendering, SegmentsReport.Segments[SegmentIdx]);
      Result := CurRendering;
    end;

  function RoadIdOf(roadblock : TRoad) : TRoadBlockId;
    begin
      if roadblock <> roadNone
        then Result := TRoadBlockId(roadblock and cRoadTopIdMask + 1)
        else Result := rbNone;
    end;

  function HighRoadIdOf(roadblock : idRoadBlock) : byte;
    begin
      Result := roadblock and cHighRoadIdMask shr 4;
    end;

  function MakeRoadBlockOf(topid : TRoadBlockId; highid : byte) : idRoadBlock;
    begin
      Result := TRoad(highid) shl 4 or (ord(topid) - 1);
    end;

  function RoadBlockId(topolid : TRoadBlockId; landid : idLand; onconcrete, onrailroad, isdummy : boolean) : TRoad;
    var
      topolidord : integer;
      LdT        : TLandType;
      horizroad  : boolean;
    begin
      if topolid <> rbNone
        then
          begin
            topolidord := ord(topolid) - 1;
            horizroad := (topolid = rbWERoad) or (topolid = rbWERoadStart) or (topolid = rbWERoadEnd);
            if LandClassOf(landId) = lncZoneD
              then
                begin
                  LdT := LandTypeOf(landId);
                  case LdT of
                    ldtN:
                      Result := topolidord or cNorthBridge shl cLandTypeShift;
                    ldtS:
                      Result := topolidord or cSouthBridge shl cLandTypeShift;
                    ldtE:
                      Result := topolidord or cEastBridge shl cLandTypeShift;
                    ldtW:
                      Result := topolidord or cWestBridge shl cLandTypeShift;
                    ldtNEo:
                      if horizroad
                        then Result := topolidord or cEastBridge shl cLandTypeShift
                        else Result := topolidord or cNorthBridge shl cLandTypeShift;
                    ldtSEo:
                      if horizroad
                        then Result := topolidord or cEastBridge shl cLandTypeShift
                        else Result := topolidord or cSouthBridge shl cLandTypeShift;
                    ldtSWo:
                      if horizroad
                        then Result := topolidord or cWestBridge shl cLandTypeShift
                        else Result := topolidord or cSouthBridge shl cLandTypeShift;
                    ldtNWo:
                      if horizroad
                        then Result := topolidord or cWestBridge shl cLandTypeShift
                        else Result := topolidord or cNorthBridge shl cLandTypeShift;
                    ldtCenter, ldtNEi, ldtSEi, ldtSWi, ldtNWi:
                      Result := topolidord or cFullBridge shl cLandTypeShift;
                    else Result := topolidord;
                  end;
                end
              else
                if onconcrete
                  then
                    if onrailroad
                      then Result := topolidord or cUrbanLevelPass shl cLandTypeShift
                      else Result := topolidord or cUrbanRoad shl cLandTypeShift
                  else
                    if onrailroad
                      then Result := topolidord or cLevelPass shl cLandTypeShift
                      else Result := topolidord;
            if isdummy
              then Result := Result or cDummyRoadMask;
          end
        else Result := roadNone;
    end;

  function IsBridge(roadblock : TRoad) : boolean;
    begin
      Result := (roadblock and cHighRoadIdMask shr 4) in [cNorthBridge..cFullBridge];
    end;
    
  function RotateRoadBlockId(id : integer; rotation : TRotation) : integer;
    const
      cRotatedRoadTopIds : array [TRotation, TRoadBlockId] of TRoadBlockId =
        (
          (
            rbNone, rbNSRoadStart, rbNSRoadEnd, rbWERoadStart, rbWERoadEnd, rbNSRoad,
            rbWERoad, rbLeftPlug, rbRightPlug, rbTopPlug, rbBottomPlug, rbCornerW,
            rbCornerS, rbCornerN, rbCornerE, rbCrossRoads
          ),
          (
            rbNone, rbWERoadStart, rbWERoadEnd, rbNSRoadEnd, rbNSRoadStart, rbWERoad,
            rbNSRoad, rbBottomPlug, rbTopPlug, rbLeftPlug, rbRightPlug, rbCornerS,
            rbCornerE, rbCornerW, rbCornerN, rbCrossRoads
          ),
          (
            rbNone, rbNSRoadEnd, rbNSRoadStart, rbWERoadEnd, rbWERoadStart, rbNSRoad,
            rbWERoad, rbRightPlug, rbLeftPlug, rbBottomPlug, rbTopPlug, rbCornerE,
            rbCornerN, rbCornerS, rbCornerW, rbCrossRoads
          ),
          (
            rbNone, rbWERoadEnd, rbWERoadStart, rbNSRoadStart, rbNSRoadEnd, rbWERoad,
            rbNSRoad, rbTopPlug, rbBottomPlug, rbRightPlug, rbLeftPlug, rbCornerN,
            rbCornerW, rbCornere, rbCornerS, rbCrossRoads
          )
        );
    var
      topid  : TRoadBlockId;
      highid : byte;
    begin
      topid := RoadIdOf(id);
      highid := HighRoadIdOf(id);
      case highid of
        cNorthBridge:
          case rotation of
            drNorth: ;
            drEast:
              highid := cWestBridge;
            drSouth:
              highid := cSouthBridge;
            drWest:
              highid := cEastBridge;
          end;
        cSouthBridge:
          case rotation of
            drNorth: ;
            drEast:
              highid := cEastBridge;
            drSouth:
              highid := cNorthBridge;
            drWest:
              highid := cWestBridge;
          end;
        cEastBridge:
          case rotation of
            drNorth: ;
            drEast:
              highid := cNorthBridge;
            drSouth:
              highid := cWestBridge;
            drWest:
              highid := cSouthBridge;
          end;
        cWestBridge:
          case rotation of
            drNorth: ;
            drEast:
              highid := cSouthBridge;
            drSouth:
              highid := cEastBridge;
            drWest:
              highid := cNorthBridge;
          end;
        else ;
      end;
      topid := cRotatedRoadTopIds[rotation, topid];
      Result := MakeRoadBlockOf(topid, highid);
    end;

end.

