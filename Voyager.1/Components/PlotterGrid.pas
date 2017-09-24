unit PlotterGrid;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, VisualControls, InternationalizerComponent;

const
  PointCount = 12;

type
  TPlotter = class(TVisualControl)
    Grid: TImage;
    year1: TLabel;
    year2: TLabel;
    year3: TLabel;
    year4: TLabel;
    year5: TLabel;
    year6: TLabel;
    year7: TLabel;
    year8: TLabel;
    year9: TLabel;
    year10: TLabel;
    year11: TLabel;
    year12: TLabel;
    val1: TLabel;
    val2: TLabel;
    val3: TLabel;
    val4: TLabel;
    val5: TLabel;
    val6: TLabel;
    val7: TLabel;
    val8: TLabel;
    val9: TLabel;
    val10: TLabel;
    val11: TLabel;
    val12: TLabel;
    Lines: TImage;
    InternationalizerComponent1: TInternationalizerComponent;
  private
    values : array[0..PointCount - 1] of TLabel;
    years  : array[0..PointCount - 1] of TLabel;
  public
    procedure Chart( info : string; EndDate : integer; Money, Kilos : boolean );
  protected
    procedure Loaded; override;
  end;

var
  Plotter: TPlotter;

implementation

  uses
    MathUtils, CompStringsParser;

  {$R *.DFM}

  procedure TPlotter.Chart( info : string; EndDate : integer; Money, Kilos : boolean );
    var
      PtData : array[0..PointCount - 1] of integer;
      Max    : integer;
      Min    : integer;
      count  : integer;
      pos    : integer;
      val    : integer;
      i, j   : integer;
      R1, R2 : TRect;
    begin
      try
        years[0]  := year1;
        years[1]  := year2;
        years[2]  := year3;
        years[3]  := year4;
        years[4]  := year5;
        years[5]  := year6;
        years[6]  := year7;
        years[7]  := year8;
        years[8]  := year9;
        years[9]  := year10;
        years[10] := year11;
        years[11] := year12;
        values[0]  := val1;
        values[1]  := val2;
        values[2]  := val3;
        values[3]  := val4;
        values[4]  := val5;
        values[5]  := val6;
        values[6]  := val7;
        values[7]  := val8;
        values[8]  := val9;
        values[9]  := val10;
        values[10] := val11;
        values[11] := val12;
        Lines.Picture.Bitmap := TBitmap.Create;
        Lines.Width := Grid.Width;
        Lines.Height := Grid.Height;
        Lines.Picture.Bitmap.Width := Grid.Width + 2;
        Lines.Picture.Bitmap.Height := Grid.Height + 2;
        Lines.Top := Grid.Top;
        Lines.Left := Grid.Left;
        Lines.Transparent := true;
        Lines.Picture.Bitmap.PixelFormat := pf32bit;
        with Lines.Picture.Bitmap.Canvas do
          begin
            Brush.Color := clBlack;
            Pen.Style   := psClear;
            Rectangle( 0, 0, Width, Height );
          end;

        // Do the dew
        pos   := 1;
        count := StrToInt(GetNextStringUpTo( info, pos, ',' ));
        min   := StrToInt(GetNextStringUpTo( info, pos, ',' ));
        max   := StrToInt(GetNextStringUpTo( info, pos, ',' ));
        for i := 0 to pred(count) do
          begin
            PtData[i] := StrToInt(GetNextStringUpTo( info, pos, ',' ));
            val := ((max - min)*PtData[i]) div 255 + min;
            if Money
              then values[i].Caption := FormatMoney( val )
              else values[i].Caption := IntToStr(val);
            if Kilos
              then values[i].Caption := values[i].Caption + 'K';
            values[i].Left    := Grid.Left + (i*Grid.Width) div pred(count) - values[i].Width div 2;
            values[i].Top     := Grid.Top + Grid.Height - Grid.Height*(PtData[i]) div 255 - values[i].Height - 3;
            values[i].Visible := true; 
            years[i].Caption  := IntToStr( EndDate - (pred(count) - i) );
            if val < 0
              then values[i].Font.Color := $000080FF //clWhite
              else values[i].Font.Color := clLime;
            if (i > 0) and (i < pred(count))
              then years[i].Caption := years[i].Caption[length(years[i].Caption) - 1] + years[i].Caption[length(years[i].Caption)];
            years[i].Left := Grid.Left + (i*Grid.Width) div pred(count) - years[i].Width div 2;
          end;
        for i := 1 to count - 1 do
          begin
            R1 := values[i].ClientRect;
            InflateRect( R1, 3, 3 );
            OffsetRect( R1, values[i].Left, values[i].Top );
            for j := 0 to i - 1 do
              begin
                R2 := values[j].ClientRect;
                InflateRect( R2, 3, 3 );
                OffsetRect( R2, values[j].Left, values[j].Top );
                values[j].Visible := not IntersectRect( R2, R1, R2 ) and values[j].Visible;
              end;
          end;
        with Lines.Picture.Bitmap.Canvas do
          begin
            Brush.Color := clBlack;
            Pen.Style   := psSolid;
            Pen.Width   := 0;
            Pen.Color   := clBlack;
            Rectangle( 0, 0, Width, Height );
            Pen.Color   := $00454637; //clGreen;
            MoveTo( (0*Grid.Width) div pred(count), Grid.Height - Grid.Height*(PtData[0]) div 255 );
            Pen.Width   := 2;
            for i := 1 to count - 1 do
              LineTo( (i*Grid.Width) div pred(count), Grid.Height - Grid.Height*(PtData[i]) div 255 );
          end;
        if (min < 0) and (max > 0)
          then
            begin
              val := -255*min div (max - min);
              with Lines.Picture.Bitmap.Canvas do
                begin
                  Pen.Color   := $000080FF; //clRed;
                  Pen.Style   := psDash;
                  Brush.Style := bsClear;
                  Pen.Width   := 1;
                  val := Grid.Height - (Grid.Height*val) div 255;
                  MoveTo( 0, val );
                  LineTo( Grid.Width, val );
                end;
            end;
        Lines.Transparent := true;
      except
      end;
    end;

  procedure TPlotter.Loaded;
    begin
      inherited;
    end;



end.


