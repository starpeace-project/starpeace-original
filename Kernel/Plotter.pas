unit Plotter;

interface

  uses
    Persistent, BackupInterfaces;

  const
    PlotterPointCount = 12;
    PlotterMaxPoint   = PlotterPointCount - 1;

  type
    PPlotterArray = ^TPlotterArray;
    TPlotterArray = array[0..PlotterMaxPoint] of byte;

  type
    TPlotter =
      class( TPersistent )
        private
          fPoints : TPlotterArray;
          fMin    : int64;
          fMax    : int64;
        private
          function GetPoints   : PPlotterArray;
          function GetMaxPoint : integer;
          function GetZero     : byte;
        public
          property Points   : PPlotterArray read GetPoints;
          property MinVal   : int64         read fMin;
          property MaxVal   : int64         read fMax;
          property Zero     : byte          read GetZero;
          property MaxPoint : integer       read GetMaxPoint;
        private
          function GetRealVal( index : integer ) : int64;
        public
          property RealVal[index : integer] : int64 read GetRealVal;
        public
          procedure Plot( Min, Max, Value : int64 );
          procedure Assign( Plotter : TPlotter ); virtual;
          function  Serialize : string;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

  procedure RegisterBackup;

implementation

  uses
    MathUtils, SysUtils;

  function TPlotter.GetPoints : PPlotterArray;
    begin
      result := @fPoints;
    end;

  function TPlotter.GetMaxPoint : integer;
    begin
      result := PlotterMaxPoint;
    end;

  function TPlotter.GetZero : byte;
    begin
      if fMin < 0
        then result := (0 - fMin)*high(byte) div (fMax - fMin)
        else result := 0;
    end;

  function TPlotter.GetRealVal( index : integer ) : int64;
    begin
      result := (MaxVal - MinVal)*Points[index] + MinVal;
    end;

  procedure TPlotter.Plot( min, max, Value : int64 );
    var
      i      : integer;
      aux    : int64;
      OldMin : int64;
      OldMax : int64;
      OldVal : int64;
    begin
      try
        move( fPoints[1], fPoints[0], MaxPoint );
        aux := min;
        min := MathUtils.minI64( min, max );
        max := MathUtils.maxI64( aux, max );
        OldMin := fMin;
        OldMax := fMax;
        fMin := MathUtils.minI64( min, fMin );
        fMax := MathUtils.maxI64( max, fMax );
        if (fMin < OldMin) or (fMax > OldMax)
          then
            for i := 0 to pred(MaxPoint) do
              begin
                OldVal := fPoints[i]*(OldMax - OldMin) div high(byte) + OldMin;
                fPoints[i] := (OldVal - fMin)*high(byte) div (fMax - fMin);
              end;
        if fMax - fMin > 0
          then fPoints[MaxPoint] := round( realmin( high(fPoints[MaxPoint]), 1.0*(Value - fMin)*high(byte)/(1.0*(fMax - fMin)) ) )
          else fPoints[MaxPoint] := 0;
      except
      end;
    end;

  procedure TPlotter.Assign( Plotter : TPlotter );
    begin
      if Plotter <> nil
        then
          begin
            move( Plotter.fPoints, fPoints, sizeof(fPoints) );
            fMin := Plotter.fMin;
            fMax := Plotter.fMax;
          end;
    end;

  function TPlotter.Serialize : string;
    var
      i : integer;
    begin
      result := IntToStr(PlotterPointCount) + ',' + IntToStr(fMin) + ',' + IntToStr(fMax) + ',';
      for i := 0 to pred(PlotterPointCount) do
        result := result + IntToStr(fPoints[i]) + ',';
    end;

  procedure TPlotter.LoadFromBackup( Reader : IBackupReader );
    var
      i : integer;
    begin
      inherited;
      fMin := Reader.ReadInt64('Min', 0);
      fMax := Reader.ReadInt64('Max', 0);
      for i := 0 to pred(PlotterPointCount) do
        fPoints[i] := Reader.ReadByte( 'p' + IntToStr(i), 0 );
    end;

  procedure TPlotter.StoreToBackup( Writer : IBackupWriter );
    var
      i   : integer;
      aux : string;
    begin
      inherited;
      Writer.WriteInt64('Min', fMin);
      Writer.WriteInt64('Max', fMax);
      for i := 0 to pred(PlotterPointCount) do
        begin
          aux := 'p' + IntToStr(i);
          Writer.WriteByte( aux, fPoints[i] );
        end;
      aux := '';
    end;


  // RegisterBackup

  procedure RegisterBackup;
    begin
      RegisterClass( TPlotter );
    end;



end.


