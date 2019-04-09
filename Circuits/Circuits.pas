unit Circuits;

interface

  uses
    Classes, Windows, Collection, BackupInterfaces, SyncObjs, ReachMatrix, Variants;

  const
    CIRCUIT_NOERROR              = 0;
    CIRCUIT_ERROR_Unknown        = 1;
    CIRCUIT_ERROR_InvalidSegment = 2;
    CIRCUIT_ERROR_AccessDenied   = 3;

  type
    TCircuitId        = integer;
    TOwnerId          = word;
    TCircuitReport    = string;
    TCircuitErrorCode = integer;

  type
    TNode       = class;
    TSegment    = class;
    TCircuit    = class;
    TCircuitMap = class;

    CNode    = class of TNode;
    CSegment = class of TSegment;
    CCircuit = class of TCircuit;

    TSegmentDirection = (segNorth, segEast, segSouth, segWest);
    PSegmentArray     = ^TSegmentArray;
    TSegmentArray     = array[TSegmentDirection] of TSegment;

    TOnRenderExtraSegInfo = function( Segment : TSegment ) : string of object;
    TOnAuthorizeBreak     = function( x, y : integer; OwnerId, BreakerId : TOwnerId ) : boolean of object;
    TOnFoundObjectsInArea = function( x1, y1, x2, y2 : integer) : boolean of object;
    TOnRenderRoadBlock    = procedure(x, y : integer) of object;

    TNode =
      class
        protected
          constructor Create( aCircuit : TCircuit ); virtual;
          procedure   Delete;
        private
          fX, fY    : word;
          fSegments : TSegmentArray;
          fDeleted  : boolean;
        private
          function GetCircuit : TCircuit;
          function GetIsEnd : boolean;
          function GetSegCount : integer;
        public
          property x : word read fX;
          property y : word read fY;
          property Segments : TSegmentArray read fSegments;
          property Circuit  : TCircuit      read GetCircuit;
          property IsEnd : boolean read GetIsEnd;
          property SegCount : integer read GetSegCount;
        private
          function  Empty : boolean;
          function  Optimizable : boolean;
          procedure Optimize;
          function  InArea( x1, y1, x2, y2 : integer ) : boolean;
          function  DistanceOf(Other : TNode) : integer;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); virtual;
          procedure StoreToBackup ( Writer : IBackupWriter ); virtual;
        public
          procedure Unplug(Seg : TSegment);
      end;

    TSegment =
      class
        protected
          constructor Create( aCircuit : TCircuit; anOwnerId : TOwnerId ); virtual;
        private
          fNodeA   : TNode;
          fNodeB   : TNode;
          fCircuit : TCircuit;
          fOwnerId : TOwnerId;
        private
          function  Intersects( x1, y1, x2, y2 : integer ) : boolean;
          function  Intersect( x1, y1, x2, y2 : integer; out intX, intY : integer ) : boolean;
          function  Overlaps( x1, y1, x2, y2 : integer ) : boolean;
          function  InArea( x1, y1, x2, y2 : integer ) : boolean;
          function  Break( x, y : integer ) : TNode;
          procedure MakeHole( x, y : integer; out A, B : TNode );
        protected
          procedure SegmentBroken( x, y : integer; NewSeg : TSegment ); virtual;
        public
          function  IsHorizontal : boolean;
          function  Length : integer;
          function  GetDeadEnd : boolean;
          procedure Unplug;
          function  GetInfluenceRect(radius : integer) : TRect;
        public
          property NodeA   : TNode    read fNodeA;
          property NodeB   : TNode    read fNodeB;
          property Circuit : TCircuit read fCircuit;
          property OwnerId : TOwnerId read fOwnerId;
          property DeadEnd :  boolean read GetDeadEnd;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); virtual;
          procedure StoreToBackup ( Writer : IBackupWriter ); virtual;
        private
          fDeleted : boolean;
      end;

    TCircuit =
      class
        protected
          constructor Create( aMap : TCircuitMap ); virtual;
        public
          destructor  Destroy; override;
        private
          fId       : TCircuitId;
          fNodes    : TCollection;
          fSegments : TCollection;
          fMap      : TCircuitMap;
        public
          property Id       : TCircuitId  read fId;
          property Nodes    : TCollection read fNodes;
          property Segments : TCollection read fSegments;
          property Map      : TCircuitMap read fMap;
        private
          procedure OnNodesModified( Operation : TCollectionOperation; Index : integer; Item : TObject );
          procedure OnSegmentsModified( Operation : TCollectionOperation; Index : integer; Item : TObject );
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); virtual;
          procedure StoreToBackup ( Writer : IBackupWriter ); virtual;
        private
          function  SegInUse(findProc : TOnFoundObjectsInArea; Seg : TSegment; dist : integer) : boolean;
        public
          function  AutoRepair : boolean;
          procedure RemoveUselessSegments(findProc : TOnFoundObjectsInArea; dist : integer);
          procedure RemoveUselessCorners (findProc : TOnFoundObjectsInArea; dist : integer);
      end;

    {$M+}
    TCircuitMap =
      class
        public
          constructor Create( anId : integer; aNodeClass : CNode; aSegmentClass : CSegment; aCircuitClass : CCircuit );
          destructor  Destroy; override;
        private
          fId           : integer;
          fCircuits     : TCollection;
          fNodeClass    : CNode;
          fSegmentClass : CSegment;
          fCircuitClass : CCircuit;
          fIgonreId     : integer;
        public
          property Id           : integer     read fId;
          property Circuits     : TCollection read fCircuits;
          property NodeClass    : CNode       read fNodeClass    write fNodeClass;
          property SegmentClass : CSegment    read fSegmentClass write fSegmentClass;
          property CircuitClass : CCircuit    read fCircuitClass write fCircuitClass;
          property IgonreId     : integer     read fIgonreId     write fIgonreId;
        private
          fOnRenderExtraSegInfo : TOnRenderExtraSegInfo;
        public
          property OnRenderExtraSegInfo : TOnRenderExtraSegInfo read fOnRenderExtraSegInfo write fOnRenderExtraSegInfo;
        public
          procedure CreateSegment( x1, y1, x2, y2 : integer; OwnerId : TOwnerId; out ErrorCode : TCircuitErrorCode ); virtual;
          procedure BreakSegmentInPoint( x, y : integer; OwnerId : TOwnerId; out ErrorCode : TCircuitErrorCode ); virtual;
          procedure NearestCircuitsToArea( Area : TRect; tolerance : integer; var Circuits : TCollection; out ErrorCode : TCircuitErrorCode ); virtual;
          function  AreasAreConnected( Area1, Area2 : TRect; tolerance : integer; out ErrorCode : TCircuitErrorCode ) : boolean; virtual;
          function  AreaIsClear( Area : TRect ) : boolean; virtual;
        published
          function SegsInArea  ( x1, y1, x2, y2 : integer ) : olevariant;
          function SegsInAreaEx( x1, y1, x2, y2 : integer ) : olevariant;
        public
          function  MixCircuits( MatchingCircuits : TCollection ) : TCircuit;
          procedure InsertSegment( A, B : TNode; Segments : TCollection; Circuit : TCircuit; OwnerId : TOwnerId; var ResultNodes : TCollection );
          procedure FindSegItersections( x1, y1, x2, y2 : integer; var Segments : TCollection );
          procedure FindOverlappedSegs( x1, y1, x2, y2 : integer; var Segments : TCollection );
          procedure FindSegsInArea( x1, y1, x2, y2 : integer; var Segments : TCollection );
          procedure CircuitsInSegs( Segs : TCollection; var Circuits : TCollection );
          procedure NodesInSegs( Segs : TCollection; var Nodes : TCollection );
          procedure InsertSortedNode( Node : TNode; Nodes : TCollection; IsHorizontal : boolean );
          function  BreakAllowed( x, y : integer; OwnerId, BreakerId : TOwnerId ) : boolean;
          procedure RemoveDummyCircuits;
          procedure RefreshArea( x1 : integer = -1; y1 : integer = -1; x2 : integer = -1; y2 : integer = -1); virtual;
          procedure Render(Notify : TOnRenderRoadBlock); virtual;
          procedure SetSize( aXsize, aYsize : integer ); virtual;
          procedure Fix; virtual;
          function  GetReachMatrix(x1, y1, x2, y2, tolerance : integer) : TReachMatrix; virtual;
          function  GetRoadOwner(x, y : integer) : integer;
        private
          fLastCircuitId    : integer;
          fDeadMeat         : TCollection;
          fMapLock          : TCriticalSection;
          fOnAuthorizeBreak : TOnAuthorizeBreak;
        public
          property OnAuthorizeBreak : TOnAuthorizeBreak read fOnAuthorizeBreak write fOnAuthorizeBreak;
        protected
          procedure DisposeDeadMeat;
          procedure Lock;
          procedure Unlock;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); virtual;
          procedure StoreToBackup ( Writer : IBackupWriter ); virtual;
        public
          function  AutoRepair : boolean;
          procedure RemoveUselessSegments(findProc : TOnFoundObjectsInArea; dist : integer);
      end;
    {$M-}

  function CanonicalizeSeg( var x1, y1, x2, y2 : integer ) : boolean;
  procedure RegisterBackup;

implementation

  uses
    SysUtils, MathUtils, BackupObjects, Logs, MetaInstances;

  const
    LineBreak = #13#10;

  function CanonicalizeSeg( var x1, y1, x2, y2 : integer ) : boolean;

    procedure SwapInts( var a, b : integer );
      var
        temp : integer;
      begin
        temp := a;
        a    := b;
        b    := temp;
      end;

    begin
      if (x1 = x2) or (y1 = y2)
        then
          begin
            if x1 > x2
              then SwapInts( x1, x2 );
            if y1 > y2
              then SwapInts( y1, y2 );
            result := true;
          end
        else result := false;
    end;

  function SegIntersect( Ax1, Ay1, Ax2, Ay2, Bx1, By1, Bx2, By2 : integer; out intX, intY : integer ) : boolean;
    begin
      if (Ay1 = Ay2) xor (By1 = By2)
        then
          if (Bx1 >= Ax1) and (Bx2 <= Ax2) and (By1 <= Ay1) and (By2 >= Ay2)
            then
              begin
                intX   := Bx1;
                intY   := Ay1;
                result := true;
              end
            else
              if (Ax1 >= Bx1) and (Ax2 <= Bx2) and (Ay1 <= By1) and (Ay2 >= By2)
                then
                  begin
                    intX   := Ax1;
                    intY   := By1;
                    result := true;
                  end
                else
                  begin
                    intX   := -1;
                    intY   := -1;
                    result := false;
                  end
        else
          begin
            intX   := -1;
            intY   := -1;
            result := false;
          end
    end;

  function SegOverlaps( Ax1, Ay1, Ax2, Ay2, Bx1, By1, Bx2, By2 : integer ) : boolean;
    begin
      result := // segments are horizontally aligned
                (Ay1 = Ay2) and (By1 = By2) and (Ay1 = By1) and
                // SegB is horizontally contained by SegA
                (((Bx1 >= Ax1) and (Bx1 <= Ax2) or (Bx2 >= Ax1) and (Bx2 <= Ax2)) or
                // SegA is horizontally contained by SegB
                 ((Bx1 >= Ax1) and (Bx1 <= Ax2) or (Bx2 >= Ax1) and (Bx2 <= Ax2))) or
                // segments are vertically aligned
                (Ax1 = Ax2) and (Bx1 = Bx2) and (Ax1 = Bx1) and
                // SegB is vertically contained by SegA
                (((By1 >= Ay1) and (By1 <= Ay2) or (By2 >= Ay1) and (By2 <= Ay2)) or
                // SegA is vertically contained by SegB
                 ((By1 >= Ay1) and (By1 <= Ay2) or (By2 >= Ay1) and (By2 <= Ay2)))
    end;


  function VerifyNode(Node : TNode) : boolean;
    var
      d   : TSegmentDirection;
      Seg : TSegment;
    begin
      if Node <> nil
        then
          try
            result := MetaInstances.ObjectIs('TNode', Node);
            for d := low(d) to high(d) do
              begin
                Seg := Node.fSegments[d];
                result := result and ((Seg = nil) or MetaInstances.ObjectIs('TSegment', Seg));
              end;
          except
            result := false;
          end
        else result := true;
    end;

  // World->Circuits->0->Circuits->482->Segments->48738->NodeB.ref
  function VerifySegment(Segment : TSegment) : boolean;
    begin
      if Segment <> nil
        then
          try
            result := MetaInstances.ObjectIs('TSegment', Segment) and
              (Segment.fNodeA <> nil) and VerifyNode(Segment.fNodeA) and
              (Segment.fNodeB <> nil) and VerifyNode(Segment.fNodeB);
          except
            result := false;
          end
        else result := true;
    end;


  // TNode

  constructor TNode.Create( aCircuit : TCircuit );
    begin
      inherited Create;
      if aCircuit <> nil
        then aCircuit.fNodes.Insert( self );
    end;

  procedure TNode.Delete;
    begin
      fDeleted := true;
    end;

  function TNode.GetCircuit : TCircuit;
    var
      dir : TSegmentDirection;
    begin
      dir    := low(dir);
      result := nil;
      while (dir <= high(dir)) and (result = nil) do
        begin
          if fSegments[dir] <> nil
            then result := fSegments[dir].Circuit;
          inc( dir );
        end;
    end;

  function TNode.GetIsEnd : boolean;
    begin
      result := SegCount = 1;
    end;

  function TNode.GetSegCount : integer;
    var
      dir : TSegmentDirection;
    begin
      result := 0;
      for dir := low(dir) to high(dir) do
        if fSegments[dir] <> nil
          then inc(result);
    end;

  function TNode.Empty : boolean;
    begin
      result := (fSegments[segNorth] = nil) and (fSegments[segEast] = nil) and (fSegments[segSouth] = nil) and (fSegments[segWest] = nil)
    end;

  function TNode.Optimizable : boolean;
    begin
      result := (fSegments[segNorth] <> nil) and (fSegments[segSouth] <> nil) and (fSegments[segEast] = nil) and (fSegments[segWest] = nil) and (fSegments[segNorth].OwnerId = fSegments[segSouth].OwnerId) or
                (fSegments[segEast] <> nil) and (fSegments[segWest] <> nil) and (fSegments[segNorth] = nil) and (fSegments[segSouth] = nil) and (fSegments[segEast].OwnerId = fSegments[segWest].OwnerId)
    end;

  procedure TNode.Optimize;
    begin
      if fSegments[segNorth] <> nil
        then
          begin
            fSegments[segSouth].fNodeA := fSegments[segNorth].fNodeA;
            fSegments[segNorth].fNodeA.fSegments[segSouth] := fSegments[segSouth];
            fSegments[segNorth].fDeleted := true;
            //fSegments[segNorth].Circuit.fSegments.Delete( fSegments[segNorth] );
            fSegments[segNorth].Circuit.fSegments.Extract( fSegments[segNorth] );
          end
        else
          begin
            fSegments[segEast].fNodeA := fSegments[segWest].fNodeA;
            fSegments[segWest].fNodeA.fSegments[segEast] := fSegments[segEast];
            fSegments[segWest].fDeleted := true;
            //fSegments[segWest].Circuit.fSegments.Delete( fSegments[segWest] );
            fSegments[segWest].Circuit.fSegments.Extract( fSegments[segWest] );
          end;
    end;
      
  function TNode.InArea( x1, y1, x2, y2 : integer ) : boolean;
    begin
      result := (x >= x1) and (y >= y1) and (x <= x2) and (y <= y2);
    end;

  function TNode.DistanceOf(Other : TNode) : integer;
    var
      dx, dy : integer;
    begin
      if fX > Other.fX
        then dx := fX - Other.fX
        else dx := Other.fX - fX;
      if fY > Other.fY
        then dy := fY - Other.fY
        else dy := Other.fY - fY;
      result := max(dx, dy);
    end;

  procedure TNode.LoadFromBackup( Reader : IBackupReader );
    var
      dir : TSegmentDirection;
    begin
      inherited;
      fX := Reader.ReadInteger( 'x', 0 );
      fY := Reader.ReadInteger( 'y', 0 );
      for dir := low(dir) to high(dir) do
        Reader.ReadObject( 'Seg' + IntToStr(integer(dir)), fSegments[dir], nil ); 
    end;

  procedure TNode.StoreToBackup( Writer : IBackupWriter );
    var
      dir : TSegmentDirection;
      aux : string;
    begin
      inherited;
      Writer.WriteInteger( 'x', fX );
      Writer.WriteInteger( 'y', fY );
      for dir := low(dir) to high(dir) do
        begin
          aux := 'Seg' + IntToStr(integer(dir));
          Writer.WriteObjectRef( aux, fSegments[dir] );
        end;
    end;

  procedure TNode.Unplug(Seg : TSegment);
    var
      dir : TSegmentDirection;
    begin
      for dir := low(dir) to high(dir) do
        if fSegments[dir] = Seg
          then fSegments[dir] := nil;
    end;


  // TSegment

  constructor TSegment.Create( aCircuit : TCircuit; anOwnerId : TOwnerId );
    begin
      inherited Create;
      fCircuit := aCircuit;
      fOwnerId := anOwnerId;
      if fCircuit <> nil
        then fCircuit.fSegments.Insert( self );
    end;

  function TSegment.Intersects( x1, y1, x2, y2 : integer ) : boolean;
    var
      useless : integer;
    begin
      result := (fNodeA <> nil) and (fNodeB <> nil) and SegIntersect( x1, y1, x2, y2, fNodeA.fX, fNodeA.fY, fNodeB.fX, fNodeB.fY, useless, useless )
    end;

  function TSegment.Intersect( x1, y1, x2, y2 : integer; out intX, intY : integer ) : boolean;
    begin
      if (fNodeA <> nil) and (fNodeB <> nil)
        then result := SegIntersect( x1, y1, x2, y2, fNodeA.fX, fNodeA.fY, fNodeB.fX, fNodeB.fY, intX, intY )
        else
          begin
            intX   := -1;
            intY   := -1;
            result := false;
          end;
    end;

  function TSegment.Overlaps( x1, y1, x2, y2 : integer ) : boolean;
    begin
      result := (fNodeA <> nil) and (fNodeB <> nil) and SegOverlaps( x1, y1, x2, y2, fNodeA.fX, fNodeA.fY, fNodeB.fX, fNodeB.fY )
    end;

  function TSegment.InArea( x1, y1, x2, y2 : integer ) : boolean;
    begin
      result := (fNodeA <> nil) and (fNodeB <> nil) and
                ((fNodeA.InArea( x1, y1, x2, y2 ) or fNodeB.InArea( x1, y1, x2, y2 )) or
                 IsHorizontal and (Intersects( x1, y1, x1, y2 ) or Intersects( x2, y1, x2, y2 )) or
                 not IsHorizontal and (Intersects( x1, y1, x2, y1 ) or Intersects( x1, y2, x2, y2 )));
    end;
    
  function TSegment.Break( x, y : integer ) : TNode;
    var
      NewSeg : TSegment;
    begin
      result := Circuit.Map.NodeClass.Create( fCircuit );
      NewSeg := Circuit.Map.SegmentClass.Create( fCircuit, fOwnerId );
      if IsHorizontal
        then
          begin
            result.fX := x;
            result.fY := fNodeA.fY;
            result.fSegments[segWest] := self;
            result.fSegments[segEast] := NewSeg;
            fNodeB.fSegments[segWest] := NewSeg;
          end
        else
          begin
            result.fY := y;
            result.fX := fNodeA.fX;
            result.fSegments[segNorth] := self;
            result.fSegments[segSouth] := NewSeg;
            fNodeB.fSegments[segNorth] := NewSeg;
          end;
      NewSeg.fNodeA := result;
      NewSeg.fNodeB := fNodeB;
      fNodeB        := result;
      SegmentBroken( x, y, NewSeg ); 
    end;

  procedure TSegment.MakeHole( x, y : integer; out A, B : TNode );
    var
      NewSeg : TSegment;
    begin
      if IsHorizontal
        then
          begin
            if (x - 1 > fNodeA.x) and (x + 1 < fNodeB.x)
              then
                begin
                  NewSeg := Circuit.Map.SegmentClass.Create( fCircuit, fOwnerId );
                  A := Circuit.Map.NodeClass.Create( fCircuit );
                  A.fX := x - 1;
                  A.fY := fNodeA.fY;
                  A.fSegments[segWest] := self;
                  B := Circuit.Map.NodeClass.Create( fCircuit );
                  B.fX := x + 1;
                  B.fY := y;
                  B.fSegments[segEast] := NewSeg;
                  fNodeB.fSegments[segWest] := NewSeg;
                  NewSeg.fNodeA := B;
                  NewSeg.fNodeB := fNodeB;
                  fNodeB := A;
                end
              else
                if (x - 1 <= fNodeA.x) and (x + 1 >= fNodeB.x)
                  then
                    begin
                      fNodeA.fSegments[segEast] := nil;
                      if fNodeA.Empty
                        then
                          begin
                            fCircuit.fNodes.Extract(fNodeA); //fCircuit.fNodes.Delete( fNodeA );
                            fNodeA.Delete;
                            A := nil
                          end
                        else A := fNodeA;
                      fNodeB.fSegments[segWest] := nil;
                      if fNodeB.Empty
                        then
                          begin
                            fCircuit.fNodes.Extract(fNodeB); //fCircuit.fNodes.Delete( fNodeB );
                            fNodeB.Delete;
                            B := nil;
                          end
                        else B := fNodeB;
                      fDeleted := true;
                      fCircuit.fSegments.Extract( self );
                      //fCircuit.fSegments.Delete( self );
                    end
                  else
                    if x - 1 <= fNodeA.x
                      then
                        begin
                          B := Circuit.Map.NodeClass.Create( fCircuit );
                          B.fX := x + 1;
                          B.fY := y;
                          B.fSegments[segEast] := self;
                          fNodeA.fSegments[segEast] := nil;
                          if fNodeA.Empty
                            then
                              begin
                                fCircuit.fNodes.Extract(fNodeA); //fCircuit.fNodes.Delete( fNodeA );
                                fNodeA.Delete;
                                A := nil
                              end
                            else A := fNodeA;
                          fNodeA := B;
                        end
                      else
                        begin
                          A := Circuit.Map.NodeClass.Create( fCircuit );
                          A.fX := x - 1;
                          A.fY := fNodeA.fY;
                          A.fSegments[segWest] := self;
                          fNodeB.fSegments[segWest] := nil;
                          if fNodeB.Empty
                            then
                              begin
                                fCircuit.fNodes.Extract(fNodeB); //fCircuit.fNodes.Delete( fNodeB );
                                fNodeB.Delete;
                                B := nil;
                              end
                            else B := fNodeB;
                          fNodeB := A;
                        end;
          end
        else
          begin
            if (y - 1 > fNodeA.y) and (y + 1 < fNodeB.y)
              then
                begin
                  NewSeg := Circuit.Map.SegmentClass.Create( fCircuit, fOwnerId );
                  A := Circuit.Map.NodeClass.Create( fCircuit );
                  A.fY := y - 1;
                  A.fX := fNodeA.fX;
                  A.fSegments[segNorth] := self;
                  B := Circuit.Map.NodeClass.Create( fCircuit );
                  B.fY := y + 1;
                  B.fX := x;
                  B.fSegments[segSouth] := NewSeg;
                  fNodeB.fSegments[segNorth] := NewSeg;
                  NewSeg.fNodeA := B;
                  NewSeg.fNodeB := fNodeB;
                  fNodeB := A;
                end
              else
                if (y - 1 <= fNodeA.y) and (y + 1 >= fNodeB.y)
                  then
                    begin
                      fNodeA.fSegments[segSouth] := nil;
                      if fNodeA.Empty
                        then
                          begin
                            fCircuit.fNodes.Extract(fNodeA); //fCircuit.fNodes.Delete( fNodeA );
                            fNodeA.Delete;
                            A := nil
                          end
                        else A := fNodeA;
                      fNodeB.fSegments[segNorth] := nil;
                      if fNodeB.Empty
                        then
                          begin
                            fCircuit.fNodes.Extract(fNodeB); //fCircuit.fNodes.Delete( fNodeB );
                            fNodeB.Delete;
                            B := nil;
                          end
                        else B := fNodeB;
                      fDeleted := true;
                      fCircuit.fSegments.Extract( self );
                      //fCircuit.fSegments.Delete( self );
                    end
                  else
                    if y - 1 <= fNodeA.y
                      then
                        begin
                          B := Circuit.Map.NodeClass.Create( fCircuit );
                          B.fY := y + 1;
                          B.fX := x;
                          B.fSegments[segSouth] := self;
                          fNodeA.fSegments[segSouth] := nil;
                          if fNodeA.Empty
                            then
                              begin
                                fCircuit.fNodes.Extract(fNodeA); //fCircuit.fNodes.Delete( fNodeA );
                                fNodeA.Delete;
                                A := nil
                              end
                            else A := fNodeA;
                          fNodeA := B;
                        end
                      else
                        begin
                          A := Circuit.Map.NodeClass.Create( fCircuit );
                          A.fY := y - 1;
                          A.fX := fNodeA.fX;
                          A.fSegments[segNorth] := self;
                          fNodeB.fSegments[segNorth] := nil;
                          if fNodeB.Empty
                            then
                              begin
                                fCircuit.fNodes.Extract(fNodeB); //fCircuit.fNodes.Delete( fNodeB );
                                fNodeB.Delete;
                                B := nil;
                              end
                            else B := fNodeB;
                          fNodeB := A;
                        end;
          end;
    end;

  procedure TSegment.SegmentBroken( x, y : integer; NewSeg : TSegment );
    begin
    end;

  function TSegment.IsHorizontal : boolean;
    begin
      result := fNodeA.fY = fNodeB.fY;
    end;

  function TSegment.Length : integer;
    begin
      if IsHorizontal
        then result := NodeB.x - NodeA.x
        else result := NodeB.y - NodeA.y;
    end;

  function TSegment.GetDeadEnd : boolean;
    begin
      result := fNodeA.IsEnd or fNodeB.IsEnd;
    end;

  procedure TSegment.Unplug;
    begin
      fNodeA.Unplug(self);
      fNodeB.Unplug(self);
    end;

  function TSegment.GetInfluenceRect(radius : integer) : TRect;
    begin
      if fNodeA.fY = fNodeB.fY
        then result := Rect(min(fNodeA.fX, fNodeB.fX), fNodeA.fY, max(fNodeA.fX, fNodeB.fX), fNodeA.fY)
        else result := Rect(fNodeA.fX, min(fNodeA.fY, fNodeB.fY), fNodeA.fX, max(fNodeA.fY, fNodeB.fY));
      InflateRect(result, radius, radius);
    end;

  procedure TSegment.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      Reader.ReadObject( 'NodeA', fNodeA, nil );
      Reader.ReadObject( 'NodeB', fNodeB, nil );
      Reader.ReadObject( 'Circuit', fCircuit, nil );
      fOwnerId := Reader.ReadInteger( 'OwnerId', 0 );
    end;

  procedure TSegment.StoreToBackup( Writer : IBackupWriter );
    begin
      Writer.WriteObjectRef( 'NodeA', fNodeA );
      Writer.WriteObjectRef( 'NodeB', fNodeB );
      Writer.WriteObjectRef( 'Circuit', fCircuit );
      Writer.WriteInteger( 'OwnerId', fOwnerId );
    end;


  // TCircuit

  constructor TCircuit.Create( aMap : TCircuitMap );
    begin
      inherited Create;
      fMap      := aMap;
      fNodes    := TNotifiedCollection.Create( 0, rkBelonguer );
      fSegments := TNotifiedCollection.Create( 0, rkBelonguer );
      TNotifiedCollection(Nodes).OnModified := OnNodesModified;
      TNotifiedCollection(Segments).OnModified := OnSegmentsModified;
      fMap.fCircuits.Insert( self );
      inc( fMap.fLastCircuitId );
      fId := fMap.fLastCircuitId;
    end;

  destructor TCircuit.Destroy;
    var
      i : integer;
    begin
      try
        Logs.Log( 'Demolition', TimeToStr(Now) + ' - ' + ClassName );
      except
      end;
      for i := pred(fNodes.Count) downto 0 do
        begin
          //fNodes.Free;
          TNode(fNodes[i]).Delete;
        end;
      fSegments.Free;
      inherited;
    end;

  procedure TCircuit.OnNodesModified( Operation : TCollectionOperation; Index : integer; Item : TObject );
    begin
    end;

  procedure TCircuit.OnSegmentsModified( Operation : TCollectionOperation; Index : integer; Item : TObject );
    begin
      case Operation of
        opDeletion :
          if fSegments.Count = 1
            then fMap.fDeadMeat.Insert( self );
      end;
    end;

  procedure TCircuit.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      fId := Reader.ReadInteger( 'Id', 0 );
      Reader.ReadObject( 'Nodes', fNodes, nil );
      Reader.ReadObject( 'Segments', fSegments, nil );
      Reader.ReadObject( 'Map', fMap, nil );
    end;

  procedure TCircuit.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteInteger( 'Id', fId );
      Writer.WriteObject( 'Nodes', fNodes );
      Writer.WriteObject( 'Segments', fSegments );
      Writer.WriteObjectRef( 'Map', fMap );
    end;

  function TCircuit.AutoRepair : boolean;
    var
      i        : integer;
      AuxSegs  : TCollection;
      AuxNodes : TCollection;
      Node     : TNode;
      Seg      : TSegment;
      d        : TSegmentDirection;
    begin
      try
        AuxSegs  := TCollection.Create(0, rkUse);
        AuxNodes := TCollection.Create(0, rkUse);
        try
          // Verify AuxNodes
          for i := pred(fNodes.Count) downto 0 do
            begin
              Node := TNode(fNodes[i]);
              if not VerifyNode(Node)
                then
                  begin
                    fNodes.AtExtract(i);
                    AuxNodes.Insert(Node);
                  end
                else
                  if Node.fDeleted or Node.Empty
                    then fNodes.AtExtract(i);
            end;
          // Verify Segments
          for i := pred(fSegments.Count) downto 0 do
            begin
              Seg := TSegment(fSegments[i]);
              if not VerifySegment(Seg)
                then
                  begin
                    fSegments.AtExtract(i);
                    AuxSegs.Insert(Seg);
                  end
                else
                  if (AuxNodes.IndexOf(Seg.fNodeA) <> noIndex) or (AuxNodes.IndexOf(Seg.fNodeB) <> noIndex)
                    then
                      begin
                        fSegments.AtExtract(i);
                        AuxSegs.Insert(Seg);
                      end;
            end;
          // Repair Nodes
          for i := pred(fNodes.Count) downto 0 do
            begin
              Node := TNode(fNodes[i]);
              for d := low(d) to high(d) do
                if AuxSegs.IndexOf(Node.fSegments[d]) <> noIndex
                  then Node.fSegments[d] := nil;
            end;
        finally
          if (AuxNodes.Count > 0) or (AuxSegs.Count > 0)
            then Logs.Log('Survival', Format('Currupt Segments: %d, Nodes: %d', [AuxNodes.Count, AuxSegs.Count]));
          AuxNodes.Free;
          AuxSegs.Free;
        end;
        result := true;
      except
        result := false;
      end;
    end;

  function TCircuit.SegInUse(findProc : TOnFoundObjectsInArea; Seg : TSegment; dist : integer) : boolean;
    var
      R : TRect;
    begin
      R := Seg.GetInfluenceRect(dist);
      result := findProc(R.Left, R.Top, R.Right, R.Bottom);
    end;

  procedure TCircuit.RemoveUselessSegments(findProc : TOnFoundObjectsInArea; dist : integer);
    var
      flg : boolean;
      i   : integer;
      Seg : TSegment;
      nA  : TNode;
      nB  : TNode;
    begin
      repeat
        flg := true;
        for i := pred(fSegments.Count) downto 0 do
          begin
            Seg := TSegment(fSegments[i]);
            if Seg.DeadEnd and not SegInUse(findProc, Seg, dist)
              then
                begin
                  nA := Seg.fNodeA;
                  nB := Seg.fNodeB;
                  Seg.Unplug;
                  if nA.Empty
                    then fNodes.Extract(nA);
                  if nB.Empty
                    then fNodes.Extract(nB);
                  fSegments.Extract(Seg);
                  flg := false;
                end;
          end;
      until flg;
    end;

  procedure TCircuit.RemoveUselessCorners(findProc : TOnFoundObjectsInArea; dist : integer);
    var
      Corners : TCollection;
      i       : integer;
      Node    : TNode;
      dir     : TSegmentDirection;
      uSegs   : integer;
    begin
      Corners := TCollection.Create(1000, rkUse);
      try
        for i := pred(fNodes.Count) downto 0 do
          begin
            Node := TNode(fNodes[i]);
            if Node.SegCount = 2
              then
                begin
                  uSegs := 0;
                  for dir := low(dir) to high(dir) do
                    if (Node.fSegments[dir] <> nil) and not SegInUse(findProc, Node.fSegments[dir], dist)
                      then inc(uSegs);
                  if uSegs = 2
                    then Corners.Insert(Node);
                end;
          end;
        // >> Continue here
      finally
        Corners.Free;
      end;
    end;

  // TCircuitMap

  constructor TCircuitMap.Create( anId : integer; aNodeClass : CNode; aSegmentClass : CSegment; aCircuitClass : CCircuit );
    begin
      inherited Create;
      fCircuits     := TCollection.Create( 0, rkBelonguer );
      fDeadMeat     := TCollection.Create( 0, rkUse );
      fId           := anId;
      fNodeClass    := aNodeClass;
      fSegmentClass := aSegmentClass;
      fCircuitClass := aCircuitClass;
      fMapLock      := TCriticalSection.Create;
    end;

  destructor TCircuitMap.Destroy;
    begin
      try
        Logs.Log( 'Demolition', TimeToStr(Now) + ' - ' + ClassName );
      except
      end;
      fCircuits.Free;
      fDeadMeat.Free;
      fMapLock.Free;
      inherited;
    end;

  procedure TCircuitMap.CreateSegment( x1, y1, x2, y2 : integer; OwnerId : TOwnerId; out ErrorCode : TCircuitErrorCode );

    procedure CreateSubSegment( A, B : TNode; OwnerId : TOwnerId; CircuitA, CircuitB : TCircuit; Nodes : TCollection; out ErrorCode : TCircuitErrorCode );
      var
        Segments : TCollection;
        Circuits : TCollection;
        Circuit  : TCircuit;
      begin
        try
          Segments := TCollection.Create( 0, rkUse );
          try
            FindSegItersections( A.x, A.y, B.x, B.y, Segments );
            Circuits := TCollection.Create( 0, rkUse );
            try
              CircuitsInSegs( Segments, Circuits );
              if (CircuitA <> nil) and (Circuits.IndexOf( CircuitA ) = NoIndex)
                then Circuits.Insert( CircuitA );
              if (CircuitB <> nil) and (Circuits.IndexOf( CircuitB ) = NoIndex)
                then Circuits.Insert( CircuitB );
              Circuit := MixCircuits( Circuits );
              InsertSegment( A, B, Segments, Circuit, OwnerId, Nodes );
            finally
              Circuits.Free;
            end;
          finally
            Segments.Free;
          end;
          ErrorCode := CIRCUIT_NOERROR;
        except
          ErrorCode := CIRCUIT_ERROR_Unknown;
        end
      end;

    procedure OptimizeNodes( Nodes : TCollection );
      var
        Node    : TNode;
        i       : integer;
        Circuit : TCircuit;
      begin
        for i := 0 to pred(Nodes.Count) do
          begin
            Node := TNode(Nodes[i]);
            if Node.Optimizable
              then
                begin
                  Circuit := Node.Circuit;
                  Node.Optimize;
                  Circuit.fNodes.Extract(Node); //Circuit.fNodes.Delete( Nodes[i] );
                  Node.Delete;
                end;
          end;
      end;

    var
      OverlappedSegs : TCollection;
      Nodes          : TCollection;
      Node           : TNode;
      NodeA, NodeB   : TNode;
      i              : integer;
      skip           : boolean;
      ResultNodes    : TCollection;
      emptynodes     : boolean;
    begin
      if (x1 <> x2) or (y1 <> y2)
        then
          begin
            Lock;
            try
              if CanonicalizeSeg( x1, y1, x2, y2 )
                then
                  begin
                    OverlappedSegs := TCollection.Create( 0, rkUse );
                    try
                      FindOverlappedSegs( x1, y1, x2, y2, OverlappedSegs );
                      Nodes := TCollection.Create( 0, rkUse );
                      try
                        NodesInSegs( OverlappedSegs, Nodes );
                        emptynodes := Nodes.Count = 0;
                        if emptynodes or (TNode(Nodes[0]).fX > x1) or (TNode(Nodes[0]).fY > y1)
                          then
                            begin
                              Node    := NodeClass.Create( nil );
                              Node.fX := x1;
                              Node.fY := y1;
                              Nodes.AtInsert( 0, Node );
                            end
                          else Nodes.AtDelete( 0 );
                        if emptynodes or (TNode(Nodes[pred(Nodes.Count)]).fX < x2) or (TNode(Nodes[pred(Nodes.Count)]).fY < y2)
                          then
                            begin
                              Node    := NodeClass.Create( nil );
                              Node.fX := x2;
                              Node.fY := y2;
                              Nodes.Insert( Node );
                            end
                          else Nodes.AtDelete( pred(Nodes.Count) );
                        skip        := false;
                        ErrorCode   := CIRCUIT_NOERROR;
                        i           := 1;
                        ResultNodes := TCollection.Create( 0, rkUse );
                        ResultNodes.InsertColl( Nodes );
                        try
                          while (i < Nodes.Count) and (ErrorCode = CIRCUIT_NOERROR) do
                            begin
                              if (y1 = y2) and (TNode(Nodes[i - 1]).fSegments[segEast] = nil) or
                                 (x1 = x2) and (TNode(Nodes[i - 1]).fSegments[segSouth] = nil)
                                then
                                  begin
                                    NodeA := TNode(Nodes[i - 1]);
                                    NodeB := TNode(Nodes[i]);
                                    CreateSubSegment( NodeA, NodeB, OwnerId, NodeA.Circuit, NodeB.Circuit, ResultNodes, ErrorCode );
                                  end;
                              skip := not skip;
                              inc( i );
                            end;
                          OptimizeNodes( ResultNodes );
                          DisposeDeadMeat;
                        finally
                          ResultNodes.Free;
                        end;
                      finally
                        Nodes.Free;
                      end;
                    finally
                      OverlappedSegs.Free;
                    end;
                    RefreshArea;
                  end
                else ErrorCode := CIRCUIT_ERROR_InvalidSegment;
            finally
              Unlock;
            end;
          end;
    end;

  procedure TCircuitMap.BreakSegmentInPoint( x, y : integer; OwnerId : TOwnerId; out ErrorCode : TCircuitErrorCode );

    procedure CollectNodes(Node : TNode; NodeList : TCollection);
      var
        dir : TSegmentDirection;
      begin
        if NodeList.IndexOf(Node) = NoIndex
          then
            begin
              NodeList.Insert(Node);
              for dir := low(dir) to high(dir) do
                if Node.fSegments[dir] <> nil
                  then
                    with Node.fSegments[dir] do
                      begin
                        if (fNodeA <> nil) and (fNodeA <> Node)
                          then CollectNodes( fNodeA, NodeList );
                        if (fNodeB <> nil) and (fNodeB <> Node)
                          then CollectNodes( fNodeB, NodeList );
                      end;
            end;
      end;

    procedure BuildNewCircuits( Lists : TCollection; OldCircuit : TCircuit );
      var
        i, j       : integer;
        List       : TCollection;
        NewCircuit : TCircuit;
        dir        : TSegmentDirection;
        Node       : TNode;
      begin
        if Lists.Count > 1
          then
            begin
              for i := 1 to pred(Lists.Count) do
                begin
                  NewCircuit := CircuitClass.Create( self );
                  List       := TCollection(Lists[i]);
                  for j := 0 to pred(List.Count) do
                    begin
                      Node := TNode(List[j]);
                      OldCircuit.fNodes.Extract(Node);
                      if NewCircuit.fNodes.IndexOf(Node) = noIndex
                        then NewCircuit.fNodes.Insert(Node)
                        else Logs.Log('Survival', DateTimeToStr(Now) + ' ERROR Duplicated Circuit found (860)');
                      with TNode(Node) do
                        for dir := low(dir) to high(dir) do
                          if (fSegments[dir] <> nil) and (fSegments[dir].fCircuit <> NewCircuit)
                            then
                              begin
                                OldCircuit.fSegments.Extract( fSegments[dir] );
                                NewCircuit.fSegments.Insert( fSegments[dir] );
                                fSegments[dir].fCircuit := NewCircuit;
                              end;
                    end;
                end;
              if OldCircuit.fSegments.Count = 0
                then fCircuits.Delete( OldCircuit );
            end;
      end;

    function PreCollectNodes(Origin, Dest : TNode; List : TCollection; radius : integer) : boolean;
      var
        dir : TSegmentDirection;
      begin
        if (Origin = Dest) or ((Origin.fX = Dest.fX) and (Origin.fY = Dest.fY))
          then result := true
          else
            if (Dest.DistanceOf(Origin) <= radius) and (List.IndexOf(Origin) = noIndex)
              then
                begin
                  result := false;
                  List.Insert(Origin);
                  for dir := low(dir) to high(dir) do
                    if Origin.fSegments[dir] <> nil
                      then
                        with Origin.fSegments[dir] do
                          begin
                            if (fNodeA <> nil) and (fNodeA <> Origin) and PreCollectNodes(fNodeA, Dest, List, radius)
                              then result := true
                              else result := (fNodeB <> nil) and (fNodeB <> Origin) and PreCollectNodes(fNodeB, Dest, List, radius);
                            if result
                              then System.Break; // There is no way to program this without a Break!!
                          end;
                end
              else result := false;
      end;

    function NodesConnected(Origin, Dest : TNode; radius : integer) : boolean;
      var
        Stack : TCollection;
      begin
        Stack := TCollection.Create(100, rkUse);
        try
          result := PreCollectNodes(Origin, Dest, Stack, radius);
        finally
          Stack.Free;
        end;
      end;

    procedure RemoveEquivalents(List : TCollection; radius : integer);
      var
        i, j : integer;
        O, D : TNode;
      begin
        i := 0;
        while i < List.Count do
          begin
            O := TNode(List[i]);
            j := i + 1;
            while j < List.Count do
              begin
                D := TNode(List[j]);
                if NodesConnected(O, D, radius)
                  then List.Extract(D)
                  else inc(j);
              end;
            inc(i);
          end;
      end;

    procedure FindNewCircuits( Nodes : TCollection; OldCircuit : TCircuit );
      var
        List  : TCollection;
        Lists : TCollection;
        i, j  : integer;
      begin
        Lists := TCollection.Create( 0, rkBelonguer );
        try
          for i := 0 to pred(Nodes.Count) do
            if not TNode(Nodes[i]).Empty
              then
                begin
                  List := TCollection.Create(64, rkUse);
                  Lists.Insert( List );
                  CollectNodes(TNode(Nodes[i]), List);
                end;
          i := 0;
          while i < Lists.Count do
            begin
              j := i + 1;
              while (j < Lists.Count) and
                    ((TCollection(Lists[j]).Count <> TCollection(Lists[i]).Count) or
                     (TCollection(Lists[j]).IndexOf( TCollection(Lists[i]).Items[0] ) = NoIndex)) do
                inc( j );
              if j < Lists.Count
                then Lists.Delete( Lists[j] );
              inc( i );
            end;
          BuildNewCircuits( Lists, OldCircuit );
        finally
          Lists.Free;
        end;
      end;

    var
      Segments   : TCollection;
      Circuit    : TCircuit;
      intX, intY : integer;
      i          : integer;
      A, B       : TNode;
      NewNodes   : TCollection;
    begin
      Lock;
      try
        try
          Segments := TCollection.Create( 0, rkUse );
          try
            FindSegItersections( x - 1, y, x + 1, y, Segments );
            FindSegItersections( x, y - 1, x, y + 1, Segments );
            if Segments.Count > 0
              then Circuit := TSegment(Segments[0]).Circuit
              else Circuit := nil;
            NewNodes := TCollection.Create( 0, rkUse );
            try
              for i := 0 to pred(Segments.Count) do
                if BreakAllowed( x, y, TSegment(Segments[i]).OwnerId, OwnerId )
                  then
                    with TSegment(Segments[i]) do
                      if (Intersect( x - 1, y, x + 1, y, intX, intY ) or
                          Intersect( x, y - 1, x, y + 1, intX, intY )) and
                          (intX = x) and (intY = y)
                        then
                          begin
                            MakeHole( x, y, A, B );
                            if (A <> nil) and (NewNodes.IndexOf( A ) = NoIndex)
                              then NewNodes.Insert( A );
                            if (B <> nil) and (NewNodes.IndexOf( B ) = NoIndex)
                              then NewNodes.Insert( B );
                          end;
              for i := pred(NewNodes.Count) downto 0 do
                if Circuit.fNodes.IndexOf( NewNodes[i] ) = NoIndex
                  then NewNodes.AtDelete( i );
              if NewNodes.Count > 1
                then
                  begin
                    RemoveEquivalents(NewNodes, 50);
                    if NewNodes.Count > 1
                      then FindNewCircuits( NewNodes, Circuit ); // >> avoid unnecesary searches
                  end;
              DisposeDeadMeat;
              ErrorCode := CIRCUIT_NOERROR;
            finally
              NewNodes.Free;
            end;
          finally
            Segments.Free;
          end;
          RefreshArea;
        except
          ErrorCode := CIRCUIT_ERROR_Unknown;
        end;
      finally
        Unlock;
      end;
    end;

  procedure TCircuitMap.NearestCircuitsToArea( Area : TRect; tolerance : integer; var Circuits : TCollection; out ErrorCode : TCircuitErrorCode );
    var
      Segments : TCollection;
    begin
      Lock;
      try
        try
          InflateRect( Area, tolerance, tolerance );
          Segments := TCollection.Create( 0, rkUse );
          try
            FindSegsInArea( Area.Left, Area.Top, Area.Right, Area.Bottom, Segments );
            CircuitsInSegs( Segments, Circuits );
            ErrorCode := CIRCUIT_NOERROR;
          finally
            Segments.Free;
          end;
        except
          ErrorCode := CIRCUIT_ERROR_Unknown;
        end;
      finally
        Unlock;
      end;
    end;

  function TCircuitMap.AreasAreConnected( Area1, Area2 : TRect; tolerance : integer; out ErrorCode : TCircuitErrorCode ) : boolean;
    var
      Circuits1, Circuits2 : TCollection;
      i                    : integer;
    begin
      Lock;
      try
        try
          Circuits1 := TCollection.Create( 0, rkUse );
          try
            NearestCircuitsToArea( Area1, tolerance, Circuits1, ErrorCode );
            if ErrorCode = CIRCUIT_NOERROR
              then
                begin
                  Circuits2 := TCollection.Create( 0, rkUse );
                  try
                    NearestCircuitsToArea( Area1, tolerance, Circuits2, ErrorCode );
                    if ErrorCode = CIRCUIT_NOERROR
                      then
                        begin
                          i := 0;
                          while (i < Circuits1.Count) and (Circuits2.IndexOf( Circuits1[i] ) = NoIndex) do
                            inc( i );
                          result := i < Circuits1.Count;
                        end
                      else result := false
                  finally
                    Circuits2.Free;
                  end;
                end
              else result := false
          finally
            Circuits1.Free;
          end;
        except
          ErrorCode := CIRCUIT_ERROR_Unknown;
          result := false;
        end;
      finally
        Unlock;
      end;
    end;

  function TCircuitMap.AreaIsClear( Area : TRect ) : boolean;
    var
      Found     : TCollection;
      ErrorCode : TCircuitErrorCode;
    begin
      try
        Found := TCollection.Create( 0, rkUse );
        try
          NearestCircuitsToArea( Area, 0, Found, ErrorCode );
          result := (ErrorCode = CIRCUIT_NOERROR) and (Found.Count = 0);
        finally
          Found.Free;
        end;
      except
        result := false;
      end;
    end;

  function TCircuitMap.SegsInArea( x1, y1, x2, y2 : integer ) : olevariant;
    var
      Segments : TCollection;
      i        : integer;
    begin
      Lock;
      try
        try
          result   := '';
          Segments := TCollection.Create( 0, rkUse );
          try
            FindSegsInArea( x1, y1, x2, y2, Segments );
            for i := 0 to pred(Segments.Count) do
              begin
                with TSegment(Segments[i]) do
                  result :=
                    result +
                    IntToStr( fNodeA.x ) + LineBreak +
                    IntToStr( fNodeA.y ) + LineBreak +
                    IntToStr( fNodeB.x ) + LineBreak +
                    IntToStr( fNodeB.y ) + LineBreak;
                if assigned(OnRenderExtraSegInfo)
                  then result := result + OnRenderExtraSegInfo( TSegment(Segments[i]) );
              end;
          finally
            Segments.Free;
          end;
        except
        end;
      finally
        Unlock;
      end;
    end;

  function TCircuitMap.SegsInAreaEx( x1, y1, x2, y2 : integer ) : olevariant;
    var
      Segments : TCollection;
      i        : integer;
    begin
      Lock;
      try
        try
          result   := '';
          Segments := TCollection.Create( 0, rkUse );
          try
            FindSegsInArea( x1, y1, x2, y2, Segments );
            for i := 0 to pred(Segments.Count) do
              with TSegment(Segments[i]) do
                result :=
                  result +
                  IntToStr( fNodeA.x ) + LineBreak +
                  IntToStr( fNodeA.y ) + LineBreak +
                  IntToStr( fNodeB.x ) + LineBreak +
                  IntToStr( fNodeB.y ) + LineBreak +
                  IntToStr( fOwnerId );
          finally
            Segments.Free;
          end;
        except
        end;
      finally
        Unlock;
      end;
    end;

  function TCircuitMap.MixCircuits(MatchingCircuits : TCollection) : TCircuit;

    function ExtractBestCircuit(TheCircuits : TCollection) : TCircuit;
      var
        i  : integer;
        C  : TCircuit;
      begin
        result := TCircuit(TheCircuits[0]);
        for i := 1 to pred(TheCircuits.Count) do
          begin
            C := TCircuit(TheCircuits[i]);
            if C.fSegments.Count > result.fSegments.Count
              then result := C;
          end;
        TheCircuits.Extract(result);
      end;

    var
      cirIdx  : integer;
      nodeIdx : integer;
      segIdx  : integer;
      Seg     : TSegment;
      CurCirc : TCircuit;
      curNode : TNode;

    begin
      Lock;
      try
        case MatchingCircuits.Count of
          0 :
            result := CircuitClass.Create( self );
          1 :
            result := TCircuit(MatchingCircuits[0]);
          else
            begin
              // Get and remove the best circuit
              result := ExtractBestCircuit(MatchingCircuits);
              // Mix all in the best circuit
              for cirIdx := 0 to pred(MatchingCircuits.Count) do
                begin
                  CurCirc := TCircuit(MatchingCircuits[cirIdx]);
                  with CurCirc do
                    begin
                      // Add nodes to the winner
                      for nodeIdx := 0 to pred(fNodes.Count) do
                        begin
                          curNode := TNode(fNodes[nodeIdx]);
                          if (result.fNodes.IndexOf(curNode) = noIndex) and not curNode.fDeleted
                            then result.fNodes.Insert(curNode)
                            else Logs.Log('Survival', DateTimeToStr(Now) + Format(' ERROR Duplicated node found %d,%d', [curNode.fX, curNode.fY]));
                        end;

                      // Extract all nodes
                      fNodes.ExtractAll;

                      // Add segments to the winner
                      for segIdx := 0 to pred(fSegments.Count) do
                        begin
                          Seg := TSegment(fSegments[segIdx]);
                          if (result.fSegments.IndexOf(Seg) = noIndex) and not Seg.fDeleted
                            then
                              begin
                                Seg.fCircuit := result;
                                result.fSegments.Insert(Seg);
                              end
                            else Logs.Log('Survival', DateTimeToStr(Now) + ' Duplicated or deleted segment found (1330)');
                        end;

                      // Extract all segments
                      fSegments.ExtractAll;
                    end;
                  fCircuits.Delete(CurCirc);
                end;
            end;
        end;
      finally
        Unlock;
      end;
    end;

{
  function TCircuitMap.MixCircuits( MatchingCircuits : TCollection ) : TCircuit;
    begin
      Lock;
      try
        case MatchingCircuits.Count of
          0 :
            result := CircuitClass.Create( self );
          1 :
            result := TCircuit(MatchingCircuits[0]);
          else
            begin
              result := CircuitClass.Create( self );
              while MatchingCircuits.Count > 0 do
                begin
                  with TCircuit(MatchingCircuits[0]) do
                    begin
                      while fNodes.Count > 0 do
                        begin
                          result.fNodes.Insert( fNodes[0] );
                          fNodes.AtExtract( 0 );
                        end;
                      while fSegments.Count > 0 do
                        begin
                          TSegment(fSegments[0]).fCircuit := result;
                          result.fSegments.Insert( fSegments[0] );
                          fSegments.AtExtract( 0 );
                        end;
                    end;
                  fCircuits.Delete( MatchingCircuits[0] );
                  MatchingCircuits.AtDelete( 0 );
                end;
            end;
        end;
      finally
        Unlock;
      end;
    end;

}
  procedure TCircuitMap.InsertSegment( A, B : TNode; Segments : TCollection; Circuit : TCircuit; OwnerId : TOwnerId; var ResultNodes : TCollection );

    procedure JoinNodes( Nodes : TCollection; IsHorizontal : boolean; Circuit : TCircuit; OwnerId : TOwnerId );
      var
        NewSeg     : TSegment;
        i          : integer;
      begin
        for i := 1 to pred(Nodes.Count) do
          begin
            NewSeg := Circuit.Map.SegmentClass.Create( Circuit, OwnerId );
            NewSeg.fNodeA := TNode(Nodes[i - 1]);
            NewSeg.fNodeB := TNode(Nodes[i]);
            if NewSeg.fNodeA.Circuit = nil
              then Circuit.Nodes.Insert( NewSeg.fNodeA );
            if NewSeg.fNodeB.Circuit = nil
              then Circuit.Nodes.Insert( NewSeg.fNodeB );
            if IsHorizontal
              then
                begin
                  NewSeg.fNodeA.fSegments[segEast] := NewSeg;
                  NewSeg.fNodeB.fSegments[segWest] := NewSeg;
                end
              else
                begin
                  NewSeg.fNodeA.fSegments[segSouth] := NewSeg;
                  NewSeg.fNodeB.fSegments[segNorth] := NewSeg;
                end;
          end;
      end;

    var
      NewNode  : TNode;
      NewNodes : TCollection;
      intX     : integer;
      intY     : integer;
      i        : integer;
    begin
      Lock;
      try
        NewNodes := TCollection.Create( 0, rkUse );
        try
          NewNodes.Insert( A );
          NewNodes.Insert( B );
          for i := 0 to pred(Segments.Count) do
            with TSegment(Segments[i]) do
              begin
                Intersect( A.fX, A.fY, B.fX, B.fY, intX, intY );
                if (fNodeA.fX = intX) and (fNodeA.fY = intY)
                  then NewNode := fNodeA
                  else
                    if (fNodeB.fX = intX) and (fNodeB.fY = intY)
                      then NewNode := fNodeB
                      else NewNode := Break( intX, intY );
                InsertSortedNode( NewNode, NewNodes, A.fY = B.fY );
              end;
          JoinNodes( NewNodes, A.fY = B.fY, Circuit, OwnerId );
          for i := 0 to pred(NewNodes.Count) do
            InsertSortedNode( TNode(NewNodes[i]), ResultNodes, A.fY = B.fY );
        finally
          NewNodes.Free;
        end;
      finally
        Unlock;
      end;
    end;

  procedure TCircuitMap.FindSegItersections( x1, y1, x2, y2 : integer; var Segments : TCollection );
    var
      i, j : integer;
      Seg  : TSegment;
    begin
      Lock;
      try
        for i := 0 to pred(fCircuits.Count) do
          for j := 0 to pred(TCircuit(fCircuits[i]).fSegments.Count) do
            begin
              Seg := TSegment(TCircuit(fCircuits[i]).fSegments[j]);
              if Seg.Intersects( x1, y1, x2, y2 ) and (Segments.IndexOf( Seg ) = NoIndex)
                then Segments.Insert( Seg );
            end;
      finally
        Unlock;
      end;
    end;

  procedure TCircuitMap.FindOverlappedSegs( x1, y1, x2, y2 : integer; var Segments : TCollection );
    var
      i, j : integer;
    begin
      Lock;
      try
        for i := 0 to pred(fCircuits.Count) do
          for j := 0 to pred(TCircuit(fCircuits[i]).fSegments.Count) do
            with TSegment(TCircuit(fCircuits[i]).fSegments[j]) do
              if Overlaps( x1, y1, x2, y2 )
                then Segments.Insert( TCircuit(fCircuits[i]).fSegments[j] );
      finally
        Unlock;
      end;
    end;

  procedure TCircuitMap.FindSegsInArea( x1, y1, x2, y2 : integer; var Segments : TCollection );
    var
      i, j    : integer;
      Circuit : TCircuit;
      Segment : TSegment;
    begin
      Lock;
      try
        for i := 0 to pred(fCircuits.Count) do
          begin
            Circuit := TCircuit(fCircuits[i]);
            for j := 0 to pred(Circuit.fSegments.Count) do
              begin
                Segment := TSegment(Circuit.fSegments[j]);
                if Segment.InArea( x1, y1, x2, y2 )
                  then Segments.Insert(Segment);
              end;
          end;
      finally
        Unlock;
      end;
    end;

  procedure TCircuitMap.CircuitsInSegs( Segs : TCollection; var Circuits : TCollection );
    var
      i   : integer;
      Seg : TSegment;
    begin
      Lock;
      try
        for i := 0 to pred(Segs.Count) do
          begin
            Seg := TSegment(Segs[i]);
            if Circuits.IndexOf(Seg.fCircuit) = NoIndex
              then Circuits.Insert(Seg.fCircuit);
          end;
      finally
        Unlock;
      end;
    end;

  procedure TCircuitMap.NodesInSegs( Segs : TCollection; var Nodes : TCollection );
    var
      i : integer;
    begin
      Lock;
      try
        for i := 0 to pred(Segs.Count) do
          with TSegment(Segs[i]) do
            begin
              if Nodes.IndexOf( fNodeA ) = NoIndex
                then InsertSortedNode( fNodeA, Nodes, fNodeA.y = fNodeB.y );
              if Nodes.IndexOf( fNodeB ) = NoIndex
                then InsertSortedNode( fNodeB, Nodes, fNodeA.y = fNodeB.y );
            end;
      finally
        Unlock;
      end;
    end;

  procedure TCircuitMap.InsertSortedNode( Node : TNode; Nodes : TCollection; IsHorizontal : boolean );
    var
      i : integer;
    begin
      Lock;
      try
        i := 0;
        while (i < Nodes.Count) and (IsHorizontal and (TNode(Nodes[i]).fX < Node.fX) or not IsHorizontal and (TNode(Nodes[i]).fY < Node.fY)) do
          inc( i );
        Nodes.AtInsert( i, Node );
        if (i < pred(Nodes.Count)) and
           ((IsHorizontal and (TNode(Nodes[i + 1]).fX = Node.fX)) or
           ((not IsHorizontal and (TNode(Nodes[i + 1]).fY = Node.fY))))
          then Nodes.AtDelete( i + 1 );
      finally
        Unlock;
      end;
    end;

  function TCircuitMap.BreakAllowed( x, y : integer; OwnerId, BreakerId : TOwnerId ) : boolean;
    begin
      result := (assigned(OnAuthorizeBreak) and OnAuthorizeBreak( x, y, OwnerId, BreakerId )) or (OwnerId = BreakerId);
    end;

  procedure TCircuitMap.RemoveDummyCircuits;
    var
      i : integer;
      C : TCircuit;
    begin
      Lock;
      try
        for i := pred(fCircuits.Count) downto 0 do
          begin
            C := TCircuit(fCircuits[i]);
            if C.fSegments.Count < 2
              then fCircuits.AtDelete(i);
          end;
      finally
        Unlock;
      end;
    end;

  procedure TCircuitMap.RefreshArea( x1 : integer = -1; y1 : integer = -1; x2 : integer = -1; y2 : integer = -1); 
    begin
    end;

  procedure TCircuitMap.Render;
    begin
    end;

  procedure TCircuitMap.SetSize( aXsize, aYsize : integer );
    begin
    end;

  procedure TCircuitMap.Fix;
    var
      OldCircuits : TCollection;
      i, j        : integer;
      Circuit     : TCircuit;
      SegCount    : integer;
      Seg         : TSegment;
      ErrorCode   : integer;
    begin
      Lock;
      try
        OldCircuits := Circuits;
        fCircuits   := TCollection.Create( 0, rkBelonguer );
        for i := 0 to pred(OldCircuits.Count) do
          begin
            Circuit  := TCircuit(OldCircuits[i]);
            SegCount := Circuit.Segments.Count;
            j        := 0;
            while j < SegCount do
              begin
                Seg := TSegment(Circuit.Segments[j]);
                if (Seg.NodeA <> nil) and (Seg.NodeB <> nil)
                  then
                    begin
                      if (fIgonreId = 0) or (Seg.fOwnerId <> fIgonreId)
                        then CreateSegment(Seg.NodeA.x, Seg.NodeA.y, Seg.NodeB.x, Seg.NodeB.y, Seg.OwnerId, ErrorCode);
                      inc(j);
                    end
                  else
                    begin
                      Circuit.Segments.AtExtract(j);
                      dec(SegCount);
                    end;
              end;
          end;
         OldCircuits.Free;
      finally
        Unlock;
      end;
    end;

  function TCircuitMap.GetReachMatrix(x1, y1, x2, y2, tolerance : integer) : TReachMatrix;
    begin
      result := TReachMatrix.Create(x2 - x1 + 1, y2 - y1 + 1);
      result.Fill(rchReach);
    end;

  function TCircuitMap.GetRoadOwner(x, y : integer) : integer;
    var
      Segments : TCollection;
      Seg      : TSegment;
    begin
      Lock;
      try
        try
          Segments := TCollection.Create( 0, rkUse );
          try
            FindSegsInArea(x, y, x + 1, y + 1, Segments);
            if Segments.Count > 0
              then
                begin
                  Seg := TSegment(Segments[0]);
                  result := Seg.fOwnerId;
                end
              else result := -1;
          finally
            Segments.Free;
          end;
        except
          result := -1;
        end;
      finally
        Unlock;
      end;
    end;

  function TCircuitMap.AutoRepair : boolean;
    var
      i : integer;
    begin
      try
        Lock;
        try
          for i := pred(fCircuits.Count) downto 0 do
            TCircuit(fCircuits[i]).AutoRepair;
        finally
          Unlock;
        end;
        result := true;
      except
        result := false;
      end;
    end;

  procedure TCircuitMap.RemoveUselessSegments(findProc : TOnFoundObjectsInArea; dist : integer);
    var
      i       : integer;
      Circuit : TCircuit;
    begin
      Lock;
      try
        for i := pred(fCircuits.Count) downto 0 do
          begin
            Circuit := TCircuit(fCircuits[i]);
            if Circuit.fSegments.Count > 1
              then Circuit.RemoveUselessSegments(findProc, dist);
            if Circuit.fSegments.Count <= 1
              then fCircuits.Extract(Circuit);
          end;
      finally
        Unlock;
      end;
    end;

  procedure TCircuitMap.DisposeDeadMeat;
    begin
      Lock;
      try
        while fDeadMeat.Count > 0 do
          begin
            // fCircuits.Delete( fDeadMeat[0] ); // >> To avoid corruptions and mem segmentation
            fCircuits.Extract( fDeadMeat[0] );
            fDeadMeat.AtDelete( 0 );
          end;
      finally
        Unlock;
      end;
    end;

  procedure TCircuitMap.Lock;
    begin
      fMapLock.Enter;
    end;

  procedure TCircuitMap.Unlock;
    begin
      fMapLock.Leave;
    end;

  procedure TCircuitMap.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      fId := Reader.ReadInteger( 'Id', 0 );
      Reader.ReadObject( 'Circuits', fCircuits, nil );
      fLastCircuitId := Reader.ReadInteger( 'LastCircuitId', 0 );
      fDeadMeat := TCollection.Create( 0, rkUse );
      fMapLock := TCriticalSection.Create;
    end;

  procedure TCircuitMap.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Lock;
      try
        Writer.WriteInteger( 'Id', fId );
        Writer.WriteObject( 'Circuits', fCircuits );
        Writer.WriteInteger( 'LastCircuitId', fLastCircuitId );
      finally
        Unlock;
      end;
    end;


  // TNodeBackupAgent

  type
    TNodeBackupAgent =
      class(TBackupAgent)
        protected
          class procedure Write( Stream : IBackupWriter; Obj : TObject ); override;
          class procedure Read ( Stream : IBackupReader; Obj : TObject ); override;
      end;

  class procedure TNodeBackupAgent.Write( Stream : IBackupWriter; Obj : TObject );
    begin
      TNode(Obj).StoreToBackup( Stream );
    end;

  class procedure TNodeBackupAgent.Read( Stream : IBackupReader; Obj : TObject );
    begin
      TNode(Obj).LoadFromBackup( Stream );
    end;


  // TSegmentBackupAgent

  type
    TSegmentBackupAgent =
      class(TBackupAgent)
        protected
          class procedure Write( Stream : IBackupWriter; Obj : TObject ); override;
          class procedure Read ( Stream : IBackupReader; Obj : TObject ); override;
      end;

  class procedure TSegmentBackupAgent.Write( Stream : IBackupWriter; Obj : TObject );
    begin
      TSegment(Obj).StoreToBackup( Stream );
    end;

  class procedure TSegmentBackupAgent.Read( Stream : IBackupReader; Obj : TObject );
    begin
      TSegment(Obj).LoadFromBackup( Stream );
    end;


  // TCircuitBackupAgent

  type
    TCircuitBackupAgent =
      class(TBackupAgent)
        protected
          class procedure Write( Stream : IBackupWriter; Obj : TObject ); override;
          class procedure Read ( Stream : IBackupReader; Obj : TObject ); override;
      end;

  class procedure TCircuitBackupAgent.Write( Stream : IBackupWriter; Obj : TObject );
    begin
      TCircuit(Obj).StoreToBackup( Stream );
    end;

  class procedure TCircuitBackupAgent.Read( Stream : IBackupReader; Obj : TObject );
    begin
      TCircuit(Obj).LoadFromBackup( Stream );
    end;


  // TCircuitMapBackupAgent

  type
    TCircuitMapBackupAgent =
      class(TBackupAgent)
        protected
          class procedure Write( Stream : IBackupWriter; Obj : TObject ); override;
          class procedure Read ( Stream : IBackupReader; Obj : TObject ); override;
      end;

  class procedure TCircuitMapBackupAgent.Write( Stream : IBackupWriter; Obj : TObject );
    begin
      TCircuitMap(Obj).StoreToBackup( Stream );
    end;

  class procedure TCircuitMapBackupAgent.Read( Stream : IBackupReader; Obj : TObject );
    begin
      TCircuitMap(Obj).LoadFromBackup( Stream );
    end;


  // RegisterBackup

  procedure RegisterBackup;
    begin
      TNodeBackupAgent.Register( [TNode] );
      TSegmentBackupAgent.Register( [TSegment] );
      TCircuitBackupAgent.Register( [TCircuit] );
      TCircuitMapBackupAgent.Register( [TCircuitMap] );
    end;

end.



