unit Surfaces;

interface

  uses
    Classes, Windows, Collection, SysUtils, BackupInterfaces, SyncObjs,
    Matrix, LargeMatrix;

  const
    tidClassFamily_SurfacePools = 'SurfacePools';
    tidSurfacePool_Surfaces     = 'Surfaces';
    tidSurfacePool_Integrators  = 'Integrators';

  type
    TSurfaceId    = string;
    TSurfaceValue = single;

  type
    IAreaAgent =
      interface
        function getAgentArea : TRect;
      end;

  type
    // Classes defined:

    TSurfacePool       = class;
    TSurface           = class;
    TSurfaceModifier   = class;
    TSurfaceIntegrator = class;
    TIntegratorPool    = class;

    CSurfacePool       = class of TSurfacePool;
    CSurface           = class of TSurface;
    CSurfaceModifier   = class of TSurfaceModifier;
    CSurfaceIntegrator = class of TSurfaceIntegrator;
    CIntegratorPool    = class of TIntegratorPool;

    TSurfacePool =
      class
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fSurfaces : TLockableCollection;
        private
          function GetSurface( id : TSurfaceId ) : TSurface;
        public
          property Surface[id : TSurfaceId] : TSurface read GetSurface; default;
          property Surfaces : TLockableCollection read fSurfaces;
        public
          procedure AddSurface( Surface : TSurface );
          procedure Update;
      end;

    TSurface =
      class( TObject, IMatrix )
        public
          constructor Create( anId : TSurfaceId; aName : string );
          destructor  Destroy; override;
        private
          fId   : TSurfaceId;
          fName : string;
        public
          property Id   : TSurfaceId read fId;
          property Name : string     read fName;
        private
          fModifiers : TLockableCollection;
          fMatrix    : TSingleLargeMatrix;
        public
          procedure AddModifier( Modifier : TSurfaceModifier );
          procedure DelModifier( Modifier : TSurfaceModifier );
          procedure SetSize( xSize, ySize : integer );
        private
          procedure Update;
        protected
          function GetValue( x, y : integer ) : TSurfaceValue; virtual;
          function GetAreaModifiers( Area : TRect ) : TLockableCollection; virtual;
        protected
          property AreaModifiers[Area : TRect] : TLockableCollection read GetAreaModifiers;
          property Value[x, y : integer] : TSurfaceValue read GetValue; default;
        // IMatrix
        private
          function  getCols : integer;
          function  getRows : integer;
          procedure setDimensions( n, m : integer );
          function  getElement   ( i, j : integer ) : single;
          procedure setElement   ( i, j : integer; value : single );
        // IUnknown
        private
          function QueryInterface(const IID: TGUID; out Obj): hresult; stdcall;
          function _AddRef  : integer; stdcall;
          function _Release : integer; stdcall;
      end;

    TSurfaceModifier =
      class( TObject, IAreaAgent )
        public
          constructor Create( aSurfaceId : TSurfaceId; anOrigin : TPoint; aValue, aStrength : TSurfaceValue ); virtual;
          destructor  Destroy; override;
        private
          fOrigin    : TPoint;
          fValue     : TSurfaceValue;
          fNewValue  : TSurfaceValue;
          fStrength  : TSurfaceValue;
          fSurface   : TSurface;
          fFixedArea : boolean;
        protected
          function  GetValueAt( x, y : integer ) : TSurfaceValue; virtual; abstract;
          function  GetArea : TRect;                              virtual; abstract;
          procedure SetArea( anArea : TRect );                    virtual; abstract;
          procedure Update;                                       virtual; abstract;
        private
          function  GetIntersects( anArea : TRect ) : boolean;
          function  GetModified : boolean;
          procedure SetValue( aValue : TSurfaceValue );
          procedure SetStrength( aStrength : TSurfaceValue );
        public
          property Origin                   : TPoint        read fOrigin      write fOrigin;
          property Value                    : TSurfaceValue read fValue       write SetValue;
          property Modified                 : boolean       read GetModified;
          property Area                     : TRect         read GetArea      write SetArea;
          property Strength                 : TSurfaceValue read fStrength    write SetStrength;
          property FixedArea                : boolean       read fFixedArea   write fFixedArea;
          property ValueAt[x, y : integer]  : TSurfaceValue read GetValueAt; default;
          property Intersects[Area : TRect] : boolean       read GetIntersects;
        public
          function  GetClone : TSurfaceModifier;
          procedure Delete;
        private
          procedure Render;
        public
          procedure LoadFromBackup( Reader : IBackupReader ); virtual;
          procedure StoreToBackup ( Writer : IBackupWriter ); virtual;
        private
          fToBeDeleted : boolean;
        // IAreaAgent
        private
          function getAgentArea : TRect;
        // IUnknown
        private
          function QueryInterface(const IID: TGUID; out Obj): hresult; stdcall;
          function _AddRef  : integer; stdcall;
          function _Release : integer; stdcall;
      end;

    TSurfaceIntegrator =
      class
        public
          constructor Create( aSurfaceId : TSurfaceId; anArea : TRect ); overload;
          constructor Create( aSurfaceId : TSurfaceId; aReference : IAreaAgent ); overload;
          destructor  Destroy; override;
        private
          fSurface   : TSurface;
          fArea      : TRect;
          fValueLock : TCriticalSection;
          fValue     : TSurfaceValue;
          fReference : IAreaAgent;
        private
          function GetValue : TSurfaceValue;
          function GetMedia : TSurfaceValue;
        public
          property Value : TSurfaceValue read GetValue;
          property Media : TSurfaceValue read GetMedia;
        public
          procedure Integrate;
          procedure Delete;
        public
          procedure LoadFromBackup( Reader : IBackupReader ); virtual;
          procedure StoreToBackup ( Writer : IBackupWriter ); virtual;
      end;

    TIntegratorPool =
      class
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fIntegrators : TLockableCollection;
          fNewMeat     : TLockableCollection;
          fDeadMeat    : TLockableCollection;
        public
          procedure IntegrateAll;
      end;

  type
    EIntegratorException = class( Exception );

  procedure InitSurfaces;
  procedure RegisterBackup;

implementation

  uses
    ClassStorage, MathUtils, Logs;

  const
    tidLog_Survival = 'Survival';


  // TSurfacePool

  constructor TSurfacePool.Create;
    begin
      inherited Create;
      fSurfaces := TLockableCollection.Create( 0, rkBelonguer );
    end;        
    
  destructor TSurfacePool.Destroy;
    begin
      fSurfaces.Free;
      inherited;
    end;
    
  function TSurfacePool.GetSurface( id : TSurfaceId ) : TSurface;
    var
      i : integer;
    begin
      fSurfaces.Lock;
      try
        i := 0;
        while (i < fSurfaces.Count) and (TSurface(fSurfaces[i]).Id <> id) do
          inc( i );
        if i < fSurfaces.Count
          then result := TSurface(fSurfaces[i])
          else result := nil;
      finally
        fSurfaces.Unlock;
      end;
    end;

  procedure TSurfacePool.AddSurface( Surface : TSurface );
    begin
      fSurfaces.Insert( Surface );
    end;

  procedure TSurfacePool.Update;
    var
      i : integer;
    begin
      for i := 0 to pred(fSurfaces.Count) do
        TSurface(fSurfaces[i]).Update;
    end;
    

  // TSurface

  constructor TSurface.Create( anId : TSurfaceId; aName : string );
    var
      SurfacePool : TSurfacePool;
    begin
      inherited Create;
      fId         := anId;
      fName       := aName;
      fModifiers  := TLockableCollection.Create( 0, rkBelonguer );
      SurfacePool := TSurfacePool(TheClassStorage.ClassById[tidClassFamily_SurfacePools, tidSurfacePool_Surfaces]);
      SurfacePool.AddSurface( self );
    end;

  destructor TSurface.Destroy;
    begin
      fModifiers.Free;
      if fMatrix <> nil
        then fMatrix.Free;
      inherited;
    end;

  procedure TSurface.AddModifier( Modifier : TSurfaceModifier );
    begin
      if fModifiers.IndexOf(Modifier) = noIndex
        then fModifiers.Insert( Modifier )
        else Logs.Log('Modifiers', DateTimeToStr(Now) + Format(' Mofifier repeated position %d, %d.', [Modifier.Origin.x, Modifier.Origin.y]));
    end;

  procedure TSurface.DelModifier( Modifier : TSurfaceModifier );
    begin
      //Logs.Log('Modifiers', DateTimeToStr(Now) + ' >> ' + Modifier.ClassName + ' ' + IntToStr(integer(Modifier)));
      fModifiers.Extract(Modifier);
      //fModifiers.Delete( Modifier );  // >> Otherwise kaboom!!!
    end;

  function TSurface.GetValue( x, y : integer ) : TSurfaceValue;
    var
      Modifiers : TCollection;
      i         : integer;
    begin
      if fMatrix <> nil
        then result := fMatrix[y, x]
        else
          begin
            Modifiers := AreaModifiers[Rect(x, y, x + 1, y + 1)];
            result := 0;
            for i := 0 to pred(Modifiers.Count) do
              result := result + TSurfaceModifier(Modifiers[i])[x, y];
            Modifiers.Free;
          end;
    end;

  procedure TSurface.SetSize( xSize, ySize : integer );
    begin
      fMatrix := TSingleLargeMatrix.Create( ySize, xSize, 16 );
    end;

  procedure TSurface.Update;       
    var
      i    : integer;
      SM   : TSurfaceModifier;
      Area : integer;
    begin
      try
        Area := 0;
        Logs.LogMemReport(tidLog_Survival);
        Logs.Log( tidLog_Survival, DateTimeToStr(Now) + ' <SURF ' + Name );
        fModifiers.Lock;
        try
          for i := pred(fModifiers.Count) downto 0  do
            try
              SM := TSurfaceModifier(fModifiers[i]);
              SM.Render;
              if SM.fToBeDeleted
                then DelModifier( SM )
                else
                  with SM.Area do
                    inc(Area, (Right - Left)*(Bottom - Top));
            except
              on E : Exception do
                Logs.Log( tidLog_Survival, DateTimeToStr(Now) + ' SURF ERROR ' + E.Message + ' at Modifier ' + IntToStr(i) );
            end;
        finally
          fModifiers.Unlock;
        end;
        Logs.Log( tidLog_Survival, DateTimeToStr(Now) + ' SURF ' + Name + '> Area: ' + IntToStr(Area));
        Logs.LogMemReport(tidLog_Survival);
      except
        on E : Exception do
          Logs.Log( tidLog_Survival, DateTimeToStr(Now) + ' SURF ERROR: ' + E.Message );
      end;
    end;

  function TSurface.GetAreaModifiers( Area : TRect ) : TLockableCollection;
    var
      i : integer;
    begin
      result := TLockableCollection.Create( 0, rkBelonguer );
      try
        fModifiers.Lock;
        try
          for i := 0 to pred(fModifiers.Count) do
            try
              if TSurfaceModifier(fModifiers[i]).Intersects[Area]
                then result.Insert( TSurfaceModifier(fModifiers[i]).GetClone );
            except
            end
        finally
          fModifiers.Unlock;
        end;
      except
        on E : Exception do
          Logs.Log( tidLog_Survival, DateTimeToStr(Now) + ' GetAreaModifiers ERROR: ' + E.Message );
      end;
    end;

  function TSurface.getCols : integer;
    begin
      if fMatrix <> nil
        then result := fMatrix.Cols
        else result := 0;
    end;

  function TSurface.getRows : integer;
    begin
      if fMatrix <> nil
        then result := fMatrix.Rows
        else result := 0;
    end;

  procedure TSurface.setDimensions( n, m : integer );
    begin
    end;

  function TSurface.getElement( i, j : integer ) : single;
    begin
      result := GetValue( j, i );
    end;

  procedure TSurface.setElement( i, j : integer; value : single );
    begin
    end;

  function TSurface.QueryInterface(const IID: TGUID; out Obj): hresult; stdcall;
    begin
      pointer(Obj) := nil;
      result := E_FAIL;
    end;

  function TSurface._AddRef  : integer; stdcall;
    begin
      result := 1;
    end;

  function TSurface._Release : integer; stdcall;
    begin
      result := 1;
    end;


  // TSurfaceModifier

  constructor TSurfaceModifier.Create( aSurfaceId : TSurfaceId; anOrigin : TPoint; aValue, aStrength : TSurfaceValue );
    begin
      inherited Create;
      fOrigin   := anOrigin;
      fNewValue := aValue;
      fStrength := aStrength;
      if aSurfaceId <> ''
        then
          begin
            fSurface := TSurfacePool(TheClassStorage.ClassById[tidClassFamily_SurfacePools, tidSurfacePool_Surfaces])[aSurfaceId];
            fSurface.AddModifier( self );
          end;
    end;

  destructor TSurfaceModifier.Destroy;
    begin
      inherited;         
    end;

  function TSurfaceModifier.GetIntersects( anArea : TRect ) : boolean;
    var
      useless : TRect;
    begin
      result := IntersectRect( useless, Area, anArea );
    end;

  function TSurfaceModifier.GetModified : boolean;
    begin
      result := abs(fValue - fNewValue) > 0.5; //fValue <> fNewValue;
    end;
    
  procedure TSurfaceModifier.SetValue( aValue : TSurfaceValue );
    begin
      fNewValue := aValue;
    end;

  procedure TSurfaceModifier.SetStrength( aStrength : TSurfaceValue );
    begin
      fStrength := aStrength;
    end;

  function TSurfaceModifier.GetClone : TSurfaceModifier;
    begin
      result := CSurfaceModifier(ClassType).Create( '', Origin, Value, Strength );
    end;

  procedure TSurfaceModifier.Delete;
    begin
      Value := 0;
      fToBeDeleted := true;
      {
      if fSurface <> nil
        then fSurface.DelModifier( self );
      }
    end;

  procedure TSurfaceModifier.Render;
    var
      x, y : integer;
      R    : TRect;
    begin
      if (fSurface.fMatrix <> nil) and Modified
        then         
          begin
            R := Area;
            // Remove old values
            for x := R.Left to min(R.Right - 1, fSurface.fMatrix.Cols) do
              for y := R.Top to min(R.Bottom - 1, fSurface.fMatrix.Rows) do
                fSurface.fMatrix.IncElement( y, x, -ValueAt[x, y] );
                // fSurface.fMatrix[y, x] := fSurface.fMatrix[y, x] - ValueAt[x, y];

            // Set new values
            fValue := fNewValue;
            Update;
            R := Area;
            for x := R.Left to min(R.Right - 1, fSurface.fMatrix.Cols) do
              for y := R.Top to min(R.Bottom - 1, fSurface.fMatrix.Rows) do
                fSurface.fMatrix.IncElement( y, x, ValueAt[x, y] );
                // fSurface.fMatrix[y, x] := fSurface.fMatrix[y, x] + ValueAt[x, y];
          end;
    end;

  procedure TSurfaceModifier.LoadFromBackup( Reader : IBackupReader );
    var
      Name : string;
    begin
      fOrigin.x := Reader.ReadInteger( 'Origin.x', 0 );
      fOrigin.y := Reader.ReadInteger( 'Origin.y', 0 );
      fNewValue := Reader.ReadSingle( 'Value', 0 );
      fStrength := Reader.ReadSingle( 'Strength', 0 );
      Name      := Reader.ReadString( 'Surface', '' );
      if Name <> ''
        then fSurface  := TSurfacePool(TheClassStorage.ClassById[tidClassFamily_SurfacePools, tidSurfacePool_Surfaces])[Name];
    end;

  procedure TSurfaceModifier.StoreToBackup( Writer : IBackupWriter );
    begin
      Writer.WriteInteger( 'Origin.x', fOrigin.x );
      Writer.WriteInteger( 'Origin.y', fOrigin.y );
      Writer.WriteSingle( 'Value', fNewValue );
      Writer.WriteSingle( 'Strength', fStrength );
      if fSurface <> nil
        then Writer.WriteString( 'Surface', fSurface.Id )
        else Writer.WriteString( 'Surface', '' )
    end;

  function TSurfaceModifier.getAgentArea : TRect;
    begin
      result := Area;
    end;

  function TSurfaceModifier.QueryInterface(const IID: TGUID; out Obj): hresult; stdcall;
    begin
      pointer(Obj) := nil;
      result := E_FAIL;
    end;

  function TSurfaceModifier._AddRef  : integer; stdcall;
    begin
      result := 1;
    end;

  function TSurfaceModifier._Release : integer; stdcall;
    begin
      result := 1;
    end;


  // TSurfaceIntegrator

  constructor TSurfaceIntegrator.Create( aSurfaceId : TSurfaceId; anArea : TRect );
    var
      SurfacePool    : TSurfacePool;
      IntegratorPool : TIntegratorPool;
    begin
      inherited Create;
      fValueLock  := TCriticalSection.Create;
      SurfacePool := TSurfacePool(TheClassStorage.ClassById[tidClassFamily_SurfacePools, tidSurfacePool_Surfaces]);
      fSurface    := SurfacePool[aSurfaceId];
      if fSurface <> nil
        then
          begin
            fArea := anArea;
            IntegratorPool := TIntegratorPool(TheClassStorage.ClassById[tidClassFamily_SurfacePools, tidSurfacePool_Integrators]);
            IntegratorPool.fNewMeat.Insert( self );
          end
        else raise EIntegratorException.Create( 'Cannot find surface "' + aSurfaceId + '"' );
    end;

  constructor TSurfaceIntegrator.Create( aSurfaceId : TSurfaceId; aReference : IAreaAgent );
    begin
      Create( aSurfaceId, aReference.getAgentArea );
      fReference := aReference;
    end;

  destructor TSurfaceIntegrator.Destroy;
    begin
      fValueLock.Free;
      inherited;
    end;

  function TSurfaceIntegrator.GetValue : TSurfaceValue;
    begin
      fValueLock.Enter;
      try
        result := fValue;
      finally
        fValueLock.Leave;
      end;
    end;

  function TSurfaceIntegrator.GetMedia : TSurfaceValue;
    begin
      fValueLock.Enter;
      try
        try
          result := Value/abs((fArea.Left - fArea.Right)*(fArea.Top - fArea.Bottom));
        except
          result := 0;
        end;
      finally
        fValueLock.Leave;
      end;
    end;

  procedure TSurfaceIntegrator.Integrate;
    var
      x, y    : integer;
      Sum     : TSurfaceValue;
      tmpArea : TRect;
    begin
      fValueLock.Enter;
      try
        if fReference <> nil
          then
            try
              tmpArea := fReference.getAgentArea
            except
              on E : Exception do
                begin
                  fReference := nil;
                  tmpArea    := fArea;
                  Logs.Log( tidLog_Survival, DateTimeToStr(Now) + ' ERROR getting reference area in integrator: ' + E.Message );
                end;
            end
          else tmpArea := fArea;
      finally
        fValueLock.Leave;
      end;
      tmpArea.Top    := max( 0, tmpArea.Top );
      tmpArea.Left   := max( 0, tmpArea.Left );
      tmpArea.Bottom := min( fSurface.getRows, tmpArea.Bottom );
      tmpArea.Right  := min( fSurface.getCols, tmpArea.Right );
      Sum := 0;
      for y := tmpArea.Top to tmpArea.Bottom do
        for x := tmpArea.Left to tmpArea.Right do
          Sum := Sum + fSurface.Value[x, y];
      fValueLock.Enter;
      try
        fValue := Sum;
      finally
        fValueLock.Leave;
      end;
    end;

  procedure TSurfaceIntegrator.Delete;
    var
      IntegratorPool : TIntegratorPool;
    begin
      IntegratorPool := TIntegratorPool(TheClassStorage.ClassById[tidClassFamily_SurfacePools, tidSurfacePool_Integrators]);
      IntegratorPool.fDeadMeat.Insert( self );
    end;

  procedure TSurfaceIntegrator.LoadFromBackup( Reader : IBackupReader );
    var
      SurfacePool    : TSurfacePool;
      IntegratorPool : TIntegratorPool;
      SurfaceId      : string;
    begin
      fValueLock  := TCriticalSection.Create;
      SurfacePool := TSurfacePool(TheClassStorage.ClassById[tidClassFamily_SurfacePools, tidSurfacePool_Surfaces]);
      SurfaceId   := Reader.ReadString( 'Surface', '' );
      fSurface    := SurfacePool[SurfaceId];
      if fSurface <> nil
        then
          begin
            fArea.Left   := Reader.ReadInteger( 'Area.Left', 0 );
            fArea.Top    := Reader.ReadInteger( 'Area.Top', 0 );
            fArea.Right  := Reader.ReadInteger( 'Area.Right', 0 );
            fArea.Bottom := Reader.ReadInteger( 'Area.Bottom', 0 );
            IntegratorPool := TIntegratorPool(TheClassStorage.ClassById[tidClassFamily_SurfacePools, tidSurfacePool_Integrators]);
            IntegratorPool.fNewMeat.Insert( self );
          end
        else raise EIntegratorException.Create( 'Cannot find surface."' );
    end;

  procedure TSurfaceIntegrator.StoreToBackup( Writer : IBackupWriter );
    begin
      Writer.WriteString( 'Surface', fSurface.Id );
      Writer.WriteInteger( 'Area.Left', fArea.Left );
      Writer.WriteInteger( 'Area.Top', fArea.Top );
      Writer.WriteInteger( 'Area.Right', fArea.Right );
      Writer.WriteInteger( 'Area.Bottom', fArea.Bottom );
    end;


  // TIntegratorPool

  constructor TIntegratorPool.Create;
    begin
      inherited Create;
      fIntegrators := TLockableCollection.Create( 0, rkBelonguer );
      fNewMeat     := TLockableCollection.Create( 0, rkBelonguer );
      fDeadMeat    := TLockableCollection.Create( 0, rkBelonguer );
    end;

  destructor TIntegratorPool.Destroy;
    begin
      fIntegrators.Free;
      fNewMeat.Free;
      fDeadMeat.Free;
      inherited;
    end;

  procedure TIntegratorPool.IntegrateAll;

    procedure ReleaseMeat;
      begin
        fNewMeat.Lock;
        try
          while fNewMeat.Count > 0 do
            begin
              fIntegrators.Insert( fNewMeat[0] );
              fNewMeat.AtExtract( 0 );
            end;
        finally
          fNewMeat.Unlock;
        end;
        fDeadMeat.Lock;
        try
          while fDeadMeat.Count > 0 do
            begin
              fIntegrators.Extract( fDeadMeat[0] );
              // fDeadMeat.AtDelete( 0 ); >> Garbage Collection
              fDeadMeat.AtExtract( 0 );
            end;
        finally
          fDeadMeat.Unlock;
        end;
      end;

    var
      i : integer;
    begin
      fIntegrators.Lock;
      try
        try
          ReleaseMeat;
        except
          on E : Exception do
            Logs.Log( tidLog_Survival, DateTimeToStr(Now) + ' Integrator Release Meat ERROR: ' + E.Message );
        end;
        for i := pred(fIntegrators.Count) downto 0 do
          with TSurfaceIntegrator(fIntegrators[i]) do
            try
              Integrate;
            except
              on E : Exception do
                Logs.Log( tidLog_Survival, DateTimeToStr(Now) + ' Integrator No. ' + IntToStr(i) + ' ERROR: ' + E.Message );
            end;
      finally
        fIntegrators.Unlock;
      end;
    end;


  // TSurfaceModifierBackupAgent

  type
    TSurfaceModifierBackupAgent =
      class(TBackupAgent)
        protected
          class procedure Write(Stream : IBackupWriter; Obj : TObject); override;
          class procedure Read (Stream : IBackupReader; Obj : TObject); override;
      end;

  class procedure TSurfaceModifierBackupAgent.Write(Stream : IBackupWriter; Obj : TObject);
    begin
      TSurfaceModifier(Obj).StoreToBackup(Stream);
    end;

  class procedure TSurfaceModifierBackupAgent.Read(Stream : IBackupReader; Obj : TObject);
    begin
      TSurfaceModifier(Obj).LoadFromBackup(Stream);
      if TSurfaceModifier(Obj).fSurface <> nil
        then TSurfaceModifier(Obj).fSurface.AddModifier( TSurfaceModifier(Obj) );
    end;


  // TSurfaceIntegratorBackupAgent

  type
    TSurfaceIntegratorBackupAgent =
      class(TBackupAgent)
        protected
          class procedure Write(Stream : IBackupWriter; Obj : TObject); override;
          class procedure Read (Stream : IBackupReader; Obj : TObject); override;
      end;

  class procedure TSurfaceIntegratorBackupAgent.Write(Stream : IBackupWriter; Obj : TObject);
    begin
      TSurfaceIntegrator(Obj).StoreToBackup(Stream);
    end;

  class procedure TSurfaceIntegratorBackupAgent.Read(Stream : IBackupReader; Obj : TObject);
    begin
      TSurfaceIntegrator(Obj).LoadFromBackup(Stream);
    end;


  // RegisterBackup

  procedure RegisterBackup;
    begin
      TSurfaceModifierBackupAgent.Register( [TSurfaceModifier] );
      TSurfaceIntegratorBackupAgent.Register( [TSurfaceIntegrator] );
    end;


  // InitSurfaces

  procedure InitSurfaces;
    begin
      TheClassStorage.RegisterClass(
        tidClassFamily_SurfacePools,
        tidSurfacePool_Surfaces,
        TSurfacePool.Create );
      TheClassStorage.RegisterClass(
        tidClassFamily_SurfacePools,
        tidSurfacePool_Integrators,
        TIntegratorPool.Create );
    end;

end.




