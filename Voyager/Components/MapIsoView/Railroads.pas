unit Railroads;

interface

  uses
    Windows, Circuits, VoyagerServerInterfaces, MapTypes, VCLUtils;

  type
    TRailroadBlockId =
      (
        rrbNone,
        rrbNSStart,
        rrbNSEnd,
        rrbWEStart,
        rrbWEEnd,
        rrbNS,
        rrbWE,
        rrbmNE,
        rrbmNW,
        rrbmSE,
        rrbmSW,
        rrbmEN,
        rrbmES,
        rrbmWN,
        rrbmWS,
        rrbmtN,
        rrbmtS,
        rrbmtE,
        rrbmtW,
        rrbmsNE,
        rrbmsNW,
        rrbmsSE,
        rrbmsSW,
        rrbmsEN,
        rrbmsES,
        rrbmsWN,
        rrbmsWS,
        rrbmcN,
        rrbmcS,
        rrbmcE,
        rrbmcW,
        rrbctN,
        rrbctS,
        rrbctE,
        rrbctW,
        rrbcNE,
        rrbcNW,
        rrbcSE,
        rrbcSW,
        rrbc,
        rrbNSBrClimb1,
        rrbNSBrClimb2,
        rrbNSBr,
        rrbNSBrDesc1,
        rrbNSBrDesc2,
        rrbWEBrClimb1,
        rrbWEBrClimb2,
        rrbWEBr,
        rrbWEBrDesc1,
        rrbWEBrDesc2
      );

  type
    TRailroadBlockIds = set of TRailroadBlockId;

  const
    NSUnmatchable : TRailroadBlockIds =
      [
        rrbmEN,
        rrbmES,
        rrbmWN,
        rrbmWS,
        rrbmtE,
        rrbmtW,
        rrbmsEN,
        rrbmsES,
        rrbmsWN,
        rrbmsWS,
        rrbmcE,
        rrbmcW,
        rrbNSBrClimb1,
        rrbNSBrClimb2,
        rrbNSBr,
        rrbNSBrDesc1,
        rrbNSBrDesc2,
        rrbWEBrClimb1,
        rrbWEBrClimb2,
        rrbWEBr,
        rrbWEBrDesc1,
        rrbWEBrDesc2
      ];

    NSCheckLeftMatch : TRailroadBlockIds =
      [
        rrbWEEnd,
        rrbWE
      ];

    NSCheckRightMatch : TRailroadBlockIds =
      [
        rrbWEStart,
        rrbWE
      ];

    LeftNSUnmatchable : TRailroadBlockIds =
      [
        rrbmWN, // *** bridge allows it
        rrbmWS, // *** bridge allows it
        rrbmtW, // *** bridge allows it
        rrbmsWN, // *** bridge allows it
        rrbmsWS, // *** bridge allows it
        rrbmcW, // *** bridge allows it
        rrbctN,
        rrbctS,
        rrbctE,
        rrbcNE,
        rrbcSE,
        rrbc,
        rrbNSBrClimb1,
        rrbNSBrClimb2,
        rrbNSBr,
        rrbNSBrDesc1,
        rrbNSBrDesc2,
        rrbWEBrClimb1,
        rrbWEBrClimb2,
        rrbWEBr,
        rrbWEBrDesc1,
        rrbWEBrDesc2
      ];

    WECheckUpMatch : TRailroadBlockIds =
      [
        rrbNSEnd,
        rrbNS
      ];

    WECheckDownMatch : TRailroadBlockIds =
      [
        rrbNSStart,
        rrbNS
      ];

    RightNSUnmatchable : TRailroadBlockIds =
      [
        rrbmEN, // *** bridge allows it
        rrbmES, // *** bridge allows it
        rrbmtE, // *** bridge allows it
        rrbmsEN, // *** bridge allows it
        rrbmsES, // *** bridge allows it
        rrbmcE, // *** bridge allows it
        rrbctN,
        rrbctS,
        rrbctW,
        rrbcNW,
        rrbcSW,
        rrbc,
        rrbNSBrClimb1,
        rrbNSBrClimb2,
        rrbNSBr,
        rrbNSBrDesc1,
        rrbNSBrDesc2,
        rrbWEBrClimb1,
        rrbWEBrClimb2,
        rrbWEBr,
        rrbWEBrDesc1,
        rrbWEBrDesc2
      ];

    WEUnmatchable : TRailroadBlockIds =
      [
        rrbmNE,
        rrbmNW,
        rrbmSE,
        rrbmSW,
        rrbmtN,
        rrbmtS,
        rrbmsNE,
        rrbmsNW,
        rrbmsSE,
        rrbmsSW,
        rrbmcN,
        rrbmcS,
        rrbNSBrClimb1,
        rrbNSBrClimb2,
        rrbNSBr,
        rrbNSBrDesc1,
        rrbNSBrDesc2,
        rrbWEBrClimb1,
        rrbWEBrClimb2,
        rrbWEBr,
        rrbWEBrDesc1,
        rrbWEBrDesc2
      ];

    UpWEUnmatchable : TRailroadBlockIds =
      [
        rrbmNE, // *** bridge allows it
        rrbmNW, // *** bridge allows it
        rrbmtN, // *** bridge allows it
        rrbmsNE, // *** bridge allows it
        rrbmsNW, // *** bridge allows it
        rrbmcN, // *** bridge allows it
        rrbctS,
        rrbctE,
        rrbctW,
        rrbcSE,
        rrbcSW,
        rrbc,
        rrbNSBrClimb1,
        rrbNSBrClimb2,
        rrbNSBr,
        rrbNSBrDesc1,
        rrbNSBrDesc2,
        rrbWEBrClimb1,
        rrbWEBrClimb2,
        rrbWEBr,
        rrbWEBrDesc1,
        rrbWEBrDesc2
      ];

    DownWEUnmatchable : TRailroadBlockIds =
      [
        rrbmSE, // *** bridge allows it
        rrbmSW, // *** bridge allows it
        rrbmtS, // *** bridge allows it
        rrbmsSE, // *** bridge allows it
        rrbmsSW, // *** bridge allows it
        rrbmcS,  // *** bridge allows it
        rrbctN,
        rrbctE,
        rrbctW,
        rrbcNE,
        rrbcNW,
        rrbc,
        rrbNSBrClimb1,
        rrbNSBrClimb2,
        rrbNSBr,
        rrbNSBrDesc1,
        rrbNSBrDesc2,
        rrbWEBrClimb1,
        rrbWEBrClimb2,
        rrbWEBr,
        rrbWEBrDesc1,
        rrbWEBrDesc2
      ];

    NSDownModifying : TRailroadBlockIds =
      [
        rrbWEStart,
        rrbWEEnd,
        rrbWE,
        rrbctN,
        //rrbctS,
        rrbcNE,
        rrbcNW{,
        rrbcSE,
        rrbcSW}
      ];

    NSUpModifying : TRailroadBlockIds =
      [
        rrbWEStart,
        rrbWEEnd,
        rrbWE,
        //rrbctN,
        rrbctS,
        {rrbcNE,
        rrbcNW,}
        rrbcSE,
        rrbcSW
      ];

    WELeftModifying : TRailroadBlockIds =
      [
        rrbNSStart,
        rrbNSEnd,
        rrbNS,
        rrbctE,
        rrbcNE,
        rrbcSE
      ];

    WERightModifying : TRailroadBlockIds =
      [
        rrbNSStart,
        rrbNSEnd,
        rrbNS,
        rrbctW,
        rrbcNW,
        rrbcSW
      ];

    NSModifiable : TRailroadBlockIds =
      [
        rrbNone,
        rrbNSStart,
        rrbNSEnd,
        rrbNS
      ];

    WEModifiable : TRailroadBlockIds =
      [
        rrbNone,
        rrbWEStart,
        rrbWEEnd,
        rrbWE
      ];

    NSBridgeModifiable : TRailroadBlockIds =
      [
        rrbNone,
        rrbNSStart,
        rrbNSEnd,
        rrbNS
      ];

    WEBridgeModifiable : TRailroadBlockIds =
      [
        rrbNone,
        rrbWEStart,
        rrbWEEnd,
        rrbWE
      ];

  type
    TRailroadsMap = array [0 .. 0] of TRailroadBlockId;
    PRailroadsMap = ^TRailroadsMap;

  type
    IRailroadsRendering =
      interface(ICircuitsRendering)
        function  GetRailroadId(Row, Col : integer) : TRailroadBlockId;
        procedure SetRailroadId(Row, Col : integer; Value : TRailroadBlockId);
        property RailroadIds[Row, Col : integer] : TRailroadBlockId read GetRailroadId write SetRailroadId; default;
      end;

  function RenderRailroadSegments(const SegmentsReport : TSegmentReport; Left, Top, Width, Height : integer) : ICircuitsRendering;

  const
    cUrbanRailroadBase = 128;
    
  const
    railroadNone = high(TRailroad);

  function RailroadIdOf(railroadblock : idRailroadBlock) : TRailroadBlockId;

implementation

  uses
    SysUtils;

  type
    TRailroadsRendering =
      class(TInterfacedObject, IRailroadsRendering)
        private
          fRailroadIds : PRailroadsMap;
          fTop         : integer;
          fLeft        : integer;
          fWidth       : integer;
          fHeight      : integer;
        private
          function  IsValidAddress(Row, Col : integer) : boolean;
        private // ICircuitsRendering
          function  GetRailroadId(Row, Col : integer) : TRailroadBlockId;
          procedure SetRailroadId(Row, Col : integer; Value : TRailroadBlockId);
        public
          constructor Create(Top, Left, Width, Height : integer);
          destructor  Destroy; override;
      end;

  // TRailroadsRendering

  constructor TRailroadsRendering.Create(Top, Left, Width, Height : integer);
    begin
      inherited Create;
      fTop := Top;
      fLeft := Left;
      fWidth := Width;
      fHeight := Height;
      getmem(fRailroadIds, fWidth*fHeight*sizeof(fRailroadIds[0]));
      fillchar(fRailroadIds^, fWidth*fHeight*sizeof(fRailroadIds[0]), ord(rrbNone));
    end;

  destructor TRailroadsRendering.Destroy;
    begin
      freemem(fRailroadIds);
      inherited;
    end;

  function TRailroadsRendering.IsValidAddress(Row, Col : integer) : boolean;
    begin
      Result := (Row >= fTop) and (Row < fTop + fHeight) and (Col >= fLeft) and (Col < fLeft + fWidth);
    end;

  function TRailroadsRendering.GetRailroadId(Row, Col : integer) : TRailroadBlockId;
    begin
      if IsValidAddress(Row, Col)
        then Result := fRailroadIds[(Row - fTop)*fWidth + Col - fLeft]
        else Result := rrbNone;
    end;

  procedure TRailroadsRendering.SetRailroadId(Row, Col : integer; Value : TRailroadBlockId);
    begin
      if IsValidAddress(Row, Col)
        then fRailroadIds[(Row - fTop)*fWidth + Col - fLeft] := Value;
    end;

  function RenderRailroadSegments(const SegmentsReport : TSegmentReport; Left, Top, Width, Height : integer) : ICircuitsRendering;
    var
      SegmentIdx   : integer;
      CurRendering : IRailroadsRendering;

    procedure RenderRailroadSegment(Segment : TSegmentInfo);
      var
        x    : integer;
        xmin : integer;
        xmax : integer;
        y    : integer;
        ymin : integer;
        ymax : integer;

      procedure RenderNSStart;
        begin
          case CurRendering[y, x] of
            rrbNone:
              CurRendering[y, x] := rrbNSStart;
            rrbNSEnd:
              CurRendering[y, x] := rrbNS;
            rrbWEStart:
              begin
                CurRendering[y, x + 1] := rrbmWS;
                CurRendering[y, x] := rrbcSE;
                CurRendering[y - 1, x] := rrbmNE;
              end;
            rrbWEEnd:
              begin
                CurRendering[y, x - 1] := rrbmES;
                CurRendering[y, x] := rrbcSW;
                CurRendering[y - 1, x] := rrbmNW;
              end;
            rrbWE:
              begin
                CurRendering[y, x - 1] := rrbmsES;
                CurRendering[y, x + 1] := rrbmsWS;
                CurRendering[y, x] := rrbctS;
                CurRendering[y - 1, x] := rrbmtN;
              end;
            rrbmEN, rrbmES, rrbmWN, rrbmWS: ; // error
            rrbmtE, rrbmtW: ; // error
            rrbmsEN, rrbmsES, rrbmsWN, rrbmsWS: ; // error
            rrbmcE, rrbmcW: ; // error
            rrbctN:
              begin
                CurRendering[y, x - 1] := rrbmcE;
                CurRendering[y, x + 1] := rrbmcW;
                CurRendering[y, x] := rrbc;
                CurRendering[y - 1, x] := rrbmcN;
                CurRendering[y + 1, x] := rrbmcS;
              end;
            rrbcNE:
              begin
                CurRendering[y, x + 1] := rrbmtW;
                CurRendering[y, x] := rrbctE;
                CurRendering[y - 1, x] := rrbmsNE;
                CurRendering[y + 1, x] := rrbmsSE;
              end;
            rrbcNW:
              begin
                CurRendering[y, x - 1] := rrbmtE;
                CurRendering[y, x] := rrbctW;
                CurRendering[y - 1, x] := rrbmsNW;
                CurRendering[y + 1, x] := rrbmsSW;
              end;
            rrbNSBrClimb1, rrbNSBrClimb2, rrbNSBr, rrbNSBrDesc1, rrbNSBrDesc2: ; // error
            rrbWEBrClimb1, rrbWEBrClimb2, rrbWEBr, rrbWEBrDesc1, rrbWEBrDesc2: ; // error
          end;
        end;

      procedure RenderNSBlock;
        begin
          case CurRendering[y, x] of
            rrbNone:
              CurRendering[y, x] := rrbNS;
            rrbNSStart:
              CurRendering[y, x] := rrbNS;
            rrbNSEnd:
              CurRendering[y, x] := rrbNS;
            rrbWEStart:
              begin
                CurRendering[y, x + 1] := rrbmtW;
                CurRendering[y, x] := rrbctE;
                CurRendering[y + 1, x] := rrbmsSE;
                CurRendering[y - 1, x] := rrbmsNE;
              end;
            rrbWEEnd:
              begin
                CurRendering[y, x - 1] := rrbmtE;
                CurRendering[y, x] := rrbctW;
                CurRendering[y + 1, x] := rrbmsSW;
                CurRendering[y - 1, x] := rrbmsNW;
              end;
            rrbWE:
              begin
                if CurRendering[y + 2, x] in NSBridgeModifiable
                  then CurRendering[y + 2, x] := rrbNSBrClimb1;
                if CurRendering[y + 1, x] in NSBridgeModifiable
                  then CurRendering[y + 1, x] := rrbNSBrClimb2;
                CurRendering[y, x] := rrbNSBr;
                if CurRendering[y - 1, x] in NSBridgeModifiable
                  then CurRendering[y - 1, x] := rrbNSBrDesc2;
                if CurRendering[y - 2, x] in NSBridgeModifiable
                  then CurRendering[y - 2, x] := rrbNSBrDesc1;
              end;
            rrbmEN, rrbmES, rrbmWN, rrbmWS: ; // error
            rrbmtE, rrbmtW: ; // error
            rrbmsEN, rrbmsES, rrbmsWN, rrbmsWS: ; // error
            rrbmcE, rrbmcW: ; // error
            rrbctN:
              begin
                CurRendering[y, x - 1] := rrbmcE;
                CurRendering[y, x + 1] := rrbmcW;
                CurRendering[y, x] := rrbc;
                CurRendering[y - 1, x] := rrbmcN;
                CurRendering[y + 1, x] := rrbmcS;
              end;
            rrbctS:
              begin
                CurRendering[y, x - 1] := rrbmcE;
                CurRendering[y, x + 1] := rrbmcW;
                CurRendering[y, x] := rrbc;
                CurRendering[y - 1, x] := rrbmcN;
                CurRendering[y + 1, x] := rrbmcS;
              end;
            rrbcNE:
              begin
                CurRendering[y, x + 1] := rrbmtW;
                CurRendering[y, x] := rrbctE;
                CurRendering[y - 1, x] := rrbmsNE;
                CurRendering[y + 1, x] := rrbmsSE;
              end;
            rrbcNW:
              begin
                CurRendering[y, x - 1] := rrbmtE;
                CurRendering[y, x] := rrbctW;
                CurRendering[y - 1, x] := rrbmsNW;
                CurRendering[y + 1, x] := rrbmsSW;
              end;
            rrbcSE:
              begin
                CurRendering[y, x + 1] := rrbmtW;
                CurRendering[y, x] := rrbctE;
                CurRendering[y - 1, x] := rrbmsNE;
                CurRendering[y + 1, x] := rrbmsSE;
              end;
            rrbcSW:
              begin
                CurRendering[y, x - 1] := rrbmtE;
                CurRendering[y, x] := rrbctW;
                CurRendering[y - 1, x] := rrbmsNW;
                CurRendering[y + 1, x] := rrbmsSW;
              end;
            rrbNSBrClimb1, rrbNSBrClimb2, rrbNSBr, rrbNSBrDesc1, rrbNSBrDesc2: ; // error
            rrbWEBrClimb1, rrbWEBrClimb2, rrbWEBr, rrbWEBrDesc1, rrbWEBrDesc2: ; // error
          end;
        end;

      procedure RenderNSEnd;
        begin
          case CurRendering[y, x] of
            rrbNone:
              CurRendering[y, x] := rrbNSEnd;
            rrbNSStart:
              CurRendering[y, x] := rrbNS;
            rrbWEStart:
              begin
                CurRendering[y, x + 1] := rrbmWN;
                CurRendering[y, x] := rrbcNE;
                CurRendering[y + 1, x] := rrbmSE;
              end;
            rrbWEEnd:
              begin
                CurRendering[y, x - 1] := rrbmEN;
                CurRendering[y, x] := rrbcNW;
                CurRendering[y + 1, x] := rrbmSW;
              end;
            rrbWE:
              begin
                CurRendering[y, x - 1] := rrbmsEN;
                CurRendering[y, x + 1] := rrbmsWN;
                CurRendering[y, x] := rrbctN;
                CurRendering[y + 1, x] := rrbmtS;
              end;
            rrbmEN, rrbmES, rrbmWN, rrbmWS: ; // error
            rrbmtE, rrbmtW: ; // error
            rrbmsEN, rrbmsES, rrbmsWN, rrbmsWS: ; // error
            rrbmcE, rrbmcW: ; // error
            rrbctS:
              begin
                CurRendering[y, x - 1] := rrbmcE;
                CurRendering[y, x + 1] := rrbmcW;
                CurRendering[y, x] := rrbc;
                CurRendering[y - 1, x] := rrbmcN;
                CurRendering[y + 1, x] := rrbmcS;
              end;
            rrbcSE:
              begin
                CurRendering[y, x + 1] := rrbmtW;
                CurRendering[y, x] := rrbctE;
                CurRendering[y - 1, x] := rrbmsNE;
                CurRendering[y + 1, x] := rrbmsSE;
              end;
            rrbcSW:
              begin
                CurRendering[y, x - 1] := rrbmtE;
                CurRendering[y, x] := rrbctW;
                CurRendering[y - 1, x] := rrbmsNW;
                CurRendering[y + 1, x] := rrbmsSW;
              end;
            rrbNSBrClimb1, rrbNSBrClimb2, rrbNSBr, rrbNSBrDesc1, rrbNSBrDesc2: ; // error
            rrbWEBrClimb1, rrbWEBrClimb2, rrbWEBr, rrbWEBrDesc1, rrbWEBrDesc2: ; // error
          end;
        end;

      procedure RenderWEStart;
        begin
          case CurRendering[y, x] of
            rrbNone:
              CurRendering[y, x] := rrbWEStart;
            rrbNSStart:
              begin
                CurRendering[y, x + 1] := rrbmWS;
                CurRendering[y, x] := rrbcSE;
                CurRendering[y - 1, x] := rrbmNE;
              end;
            rrbNSEnd:
              begin
                CurRendering[y, x + 1] := rrbmWN;
                CurRendering[y, x] := rrbcNE;
                CurRendering[y + 1, x] := rrbmSE;
              end;
            rrbWEEnd:
              CurRendering[y, x] := rrbWE;
            rrbNS:
              begin
                CurRendering[y, x + 1] := rrbmtW;
                CurRendering[y, x] := rrbctE;
                CurRendering[y - 1, x] := rrbmsNE;
                CurRendering[y + 1, x] := rrbmsSE;
              end;
            rrbmNE, rrbmNW, rrbmSE, rrbmSW: ; // error
            rrbmtN, rrbmtS: ; // error
            rrbmsNE, rrbmsNW, rrbmsSE, rrbmsSW: ; // error
            rrbmcN, rrbmcS: ; // error
            rrbctW:
              begin
                CurRendering[y, x - 1] := rrbmcE;
                CurRendering[y, x + 1] := rrbmcW;
                CurRendering[y, x] := rrbc;
                CurRendering[y - 1, x] := rrbmcN;
                CurRendering[y + 1, x] := rrbmcS;
              end;
            rrbcNW:
              begin
                CurRendering[y, x - 1] := rrbmsEN;
                CurRendering[y, x + 1] := rrbmsWN;
                CurRendering[y, x] := rrbctN;
                CurRendering[y + 1, x] := rrbmtS;
              end;
            rrbcSW:
              begin
                CurRendering[y, x - 1] := rrbmsES;
                CurRendering[y, x + 1] := rrbmsWS;
                CurRendering[y, x] := rrbctS;
                CurRendering[y - 1, x] := rrbmtN;
              end;
            rrbNSBrClimb1, rrbNSBrClimb2, rrbNSBr, rrbNSBrDesc1, rrbNSBrDesc2: ; // error
            rrbWEBrClimb1, rrbWEBrClimb2, rrbWEBr, rrbWEBrDesc1, rrbWEBrDesc2: ; // error
          end;
        end;

      procedure RenderWEBlock;
        begin
          case CurRendering[y, x] of
            rrbNone:
              CurRendering[y, x] := rrbWE;
            rrbNSStart:
              begin
                CurRendering[y, x - 1] := rrbmsES;
                CurRendering[y, x + 1] := rrbmsWS;
                CurRendering[y, x] := rrbctS;
                CurRendering[y - 1, x] := rrbmtN;
              end;
            rrbNSEnd:
              begin
                CurRendering[y, x - 1] := rrbmsEN;
                CurRendering[y, x + 1] := rrbmsWN;
                CurRendering[y, x] := rrbctN;
                CurRendering[y + 1, x] := rrbmtS;
              end;
            rrbWEStart:
              CurRendering[y, x] := rrbWE;
            rrbWEEnd:
              CurRendering[y, x] := rrbWE;
            rrbNS:
              begin
                if CurRendering[y, x - 2] in WEBridgeModifiable
                  then CurRendering[y, x - 2] := rrbWEBrClimb1;
                if CurRendering[y, x - 1] in WEBridgeModifiable
                  then CurRendering[y, x - 1] := rrbWEBrClimb1;
                CurRendering[y, x] := rrbWEBr;
                if CurRendering[y, x + 1] in WEBridgeModifiable
                  then CurRendering[y, x + 1] := rrbWEBrDesc2;
                if CurRendering[y, x + 2] in WEBridgeModifiable
                  then CurRendering[y, x + 2] := rrbWEBrDesc1;
              end;
            rrbmNE, rrbmNW, rrbmSE, rrbmSW: ; // error
            rrbmtN, rrbmtS: ; // error
            rrbmsNE, rrbmsNW, rrbmsSE, rrbmsSW: ; // error
            rrbmcN, rrbmcS: ; // error
            rrbctE:
              begin
                CurRendering[y, x - 1] := rrbmcE;
                CurRendering[y, x + 1] := rrbmcW;
                CurRendering[y, x] := rrbc;
                CurRendering[y - 1, x] := rrbmcN;
                CurRendering[y + 1, x] := rrbmcS;
              end;
            rrbctW:
              begin
                CurRendering[y, x - 1] := rrbmcE;
                CurRendering[y, x + 1] := rrbmcW;
                CurRendering[y, x] := rrbc;
                CurRendering[y - 1, x] := rrbmcN;
                CurRendering[y + 1, x] := rrbmcS;
              end;
            rrbcNE:
              begin
                CurRendering[y, x - 1] := rrbmsEN;
                CurRendering[y, x + 1] := rrbmsWN;
                CurRendering[y, x] := rrbctN;
                CurRendering[y + 1, x] := rrbmtS;
              end;
            rrbcNW:
              begin
                CurRendering[y, x - 1] := rrbmsEN;
                CurRendering[y, x + 1] := rrbmsWN;
                CurRendering[y, x] := rrbctN;
                CurRendering[y + 1, x] := rrbmtS;
              end;
            rrbcSE:
              begin
                CurRendering[y, x - 1] := rrbmsES;
                CurRendering[y, x + 1] := rrbmsWS;
                CurRendering[y, x] := rrbctS;
                CurRendering[y - 1, x] := rrbmtN;
              end;
            rrbcSW:
              begin
                CurRendering[y, x - 1] := rrbmsES;
                CurRendering[y, x + 1] := rrbmsWS;
                CurRendering[y, x] := rrbctS;
                CurRendering[y - 1, x] := rrbmtN;
              end;
            rrbNSBrClimb1, rrbNSBrClimb2, rrbNSBr, rrbNSBrDesc1, rrbNSBrDesc2: ; // error
            rrbWEBrClimb1, rrbWEBrClimb2, rrbWEBr, rrbWEBrDesc1, rrbWEBrDesc2: ; // error
          end;
        end;

      procedure RenderWEEnd;
        begin
          case CurRendering[y, x] of
            rrbNone:
              CurRendering[y, x] := rrbWEEnd;
            rrbNSStart:
              begin
                CurRendering[y, x - 1] := rrbmES;
                CurRendering[y, x] := rrbcSW;
                CurRendering[y - 1, x] := rrbmNW;
              end;
            rrbNSEnd:
              begin
                CurRendering[y, x - 1] := rrbmEN;
                CurRendering[y, x] := rrbcNW;
                CurRendering[y + 1, x] := rrbmSW;
              end;
            rrbWEEnd:
              CurRendering[y, x] := rrbWE;
            rrbNS:
              begin
                CurRendering[y, x - 1] := rrbmtE;
                CurRendering[y, x] := rrbctW;
                CurRendering[y - 1, x] := rrbmsNW;
                CurRendering[y + 1, x] := rrbmsSW;
              end;
            rrbmNE, rrbmNW, rrbmSE, rrbmSW: ; // error
            rrbmtN, rrbmtS: ; // error
            rrbmsNE, rrbmsNW, rrbmsSE, rrbmsSW: ; // error
            rrbmcN, rrbmcS: ; // error
            rrbctE:
              begin
                CurRendering[y, x - 1] := rrbmcE;
                CurRendering[y, x + 1] := rrbmcW;
                CurRendering[y, x] := rrbc;
                CurRendering[y - 1, x] := rrbmcN;
                CurRendering[y + 1, x] := rrbmcS;
              end;
            rrbcNE:
              begin
                CurRendering[y, x - 1] := rrbmsEN;
                CurRendering[y, x + 1] := rrbmsWN;
                CurRendering[y, x] := rrbctN;
                CurRendering[y + 1, x] := rrbmtS;
              end;
            rrbcSE:
              begin
                CurRendering[y, x - 1] := rrbmsES;
                CurRendering[y, x + 1] := rrbmsWS;
                CurRendering[y, x] := rrbctS;
                CurRendering[y - 1, x] := rrbmtN;
              end;
            rrbNSBrClimb1, rrbNSBrClimb2, rrbNSBr, rrbNSBrDesc1, rrbNSBrDesc2: ; // error
            rrbWEBrClimb1, rrbWEBrClimb2, rrbWEBr, rrbWEBrDesc1, rrbWEBrDesc2: ; // error
          end;
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
                      ymax := y1
                    end
                  else ymax := y2;
                y := ymin;
                RenderNSEnd;
                inc(y);
                while y < ymax do
                  begin
                    RenderNSBlock;
                    inc(y)
                  end;
                if y = ymax
                  then RenderNSStart;
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
                          xmax := x1
                        end
                      else xmax := x2;
                    x := xmin;
                    RenderWEStart;
                    inc(x);
                    while x < xmax do
                      begin
                        RenderWEBlock;
                        inc(x);
                      end;
                    if x = xmax
                      then RenderWEEnd;
                  end;
      end;

    begin
      CurRendering := TRailroadsRendering.Create(Top, Left, Width, Height);
      for SegmentIdx := 0 to SegmentsReport.SegmentCount - 1 do
        RenderRailroadSegment(SegmentsReport.Segments[SegmentIdx]);
      Result := CurRendering;
    end;

  function RailroadIdOf(railroadblock : idRailroadBlock) : TRailroadBlockId;
    begin
      if railroadblock >= cUrbanRailroadBase
        then Result := TRailroadBlockId(railroadblock - cUrbanRailroadBase + 1)
        else Result := TRailroadBlockId(railroadblock + 1);
    end;

end.
