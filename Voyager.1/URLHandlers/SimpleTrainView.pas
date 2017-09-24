unit SimpleTrainView;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Vehicles, StdCtrls, InternationalizerComponent;

type
  TSimpleTrains = class(TForm)
    Image: TImage;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    xVal: TLabel;
    yVal: TLabel;
    Count: TLabel;
    Cnt: TLabel;
    Label3: TLabel;
    Angle: TLabel;
    InternationalizerComponent1: TInternationalizerComponent;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  public
    procedure OnVehicleArrayChanged( VehicleArray : IVehicleArray );
  end;

var
  SimpleTrains: TSimpleTrains;

implementation


{$R *.DFM}

  procedure TSimpleTrains.FormCreate(Sender: TObject);
    begin
      Image.Picture.Bitmap := TBitmap.Create;
      Image.Picture.Bitmap.Width  := Image.Width;
      Image.Picture.Bitmap.Height := Image.Height;
      with Image.Canvas do
        begin
          Pen.Color   := clBlack;
          Brush.Color := clBlack;
          Rectangle( 0, 0, Image.Width, Image.Height );
        end;
    end;

  procedure TSimpleTrains.OnVehicleArrayChanged( VehicleArray : IVehicleArray );
    const
      x1 = 385;
      y1 = 360;
      x2 = 433;
      y2 = 395;

    function ConvertX( x : single ) : integer;
      begin
        result := trunc(Image.Width*(x - x1)/(x2 - x1))
      end;

    function ConvertY( y : single ) : integer;
      begin
        result := trunc(Image.Height*(y - y1)/(y2 - y1))
      end;

    var
      i      : integer;
      v      : IVehicle;
      x, y   : integer;
      dx, dy : integer;
      rad    : integer;
    begin
      Image.Picture.Bitmap.Width  := Image.Width;
      Image.Picture.Bitmap.Height := Image.Height;
      with Image.Canvas do
        begin
          Pen.Color   := clBlack;
          Brush.Color := clBlack;
          Rectangle( 0, 0, Image.Width, Image.Height );
          Pen.Width   := 1;
          Pen.Style   := psSolid;
          Pen.Color   := clGray;
          Rectangle( ConvertX(387), ConvertY(363), ConvertX(399), ConvertY(381) );
          MoveTo( ConvertX(399), ConvertY(381) );
          LineTo( ConvertX(425), ConvertY(381) );
          Rectangle( ConvertX(425), ConvertY(381), ConvertX(429), ConvertY(390) );
          Pen.Color   := clGreen;
          Brush.Color := clYellow;
          Cnt.Caption := IntToStr(VehicleArray.getVehicleCount);
          rad := ConvertX(241) - ConvertX(240.5);
          for i := 0 to pred(VehicleArray.getVehicleCount) do
            begin
              v := VehicleArray.getVehicle( i );
              x := ConvertX( v.getX );
              y := ConvertY( v.getY );
              dx := round(cos( v.getAngle )*rad);
              dy := round(sin( v.getAngle )*rad);
              if i = 0
                then
                  begin
                    Pen.Color   := clwhite;
                    Brush.Color := clwhite;
                  end
                else
                  begin
                    Pen.Color   := clYellow;
                    Brush.Color := clYellow;
                  end;
              //Ellipse( x - rad, y - rad, x + rad, y + rad );
              Pen.Width := 3;
              MoveTo( x, y );
              LineTo( x - dx, y - dy );
              MoveTo( x, y );
              LineTo( x + dx, y + dy );
              if i = 0
                then
                  begin
                    xVal.Caption := IntToStr( trunc(v.getX) );
                    yVal.Caption := IntToStr( trunc(v.getY) );
                    Angle.Caption := IntToStr( round(180*v.getAngle/pi) );
                  end;
            end;
        end;
    end;

procedure TSimpleTrains.FormResize(Sender: TObject);
  begin
    //
  end;



end.

