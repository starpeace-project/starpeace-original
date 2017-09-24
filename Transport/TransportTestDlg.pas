unit TransportTestDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Transport, ExtCtrls, Spin;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Image: TImage;
    Panel2: TPanel;
    Button1: TButton;
    SpinEdit: TSpinEdit;
    Label1: TLabel;
    HighQuality: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure Panel2Click(Sender: TObject);
  private
    fCargoSystem : TCargoSystem;
  private
    procedure RenderView;
  end;

var
  Form1: TForm1;

implementation

  {$R *.DFM}

  uses
    MatrixLayer, MathUtils;

  const
    MaxVal = 120;

  procedure TForm1.Button1Click(Sender: TObject);
    var
      i : integer;
    begin
      fCargoSystem.Free;
      fCargoSystem := TCargoSystem.Create( 100, 100 );
      fCargoSystem.AddLayer( 0, TMatrixLayer );
      for i := 1 to SpinEdit.Value do
        if true //random(2) = 0
          then fCargoSystem[0].SetPoint( 2 + random(fCargoSystem.xSize), random(fCargoSystem.ySize), random(MaxVal) )
          else fCargoSystem[0].SetPoint( random(fCargoSystem.xSize), random(fCargoSystem.ySize), -random(MaxVal) );
        
      if HighQuality.Checked
        then fCargoSystem.Render( rqHigh )
        else fCargoSystem.Render( rqLow );
      RenderView;
    end;

  procedure TForm1.FormCreate(Sender: TObject);
    var
      i : integer;
    begin
      fCargoSystem := TCargoSystem.Create( 100, 100 );
      fCargoSystem.AddLayer( 0, TMatrixLayer );
      randomize;
      for i := 1 to 3 do
        fCargoSystem[0].SetPoint( random(fCargoSystem.xSize), random(fCargoSystem.ySize), random(MaxVal) );
      fCargoSystem.Render( rqHigh );
      Image.Picture.Bitmap := TBitmap.Create;
      Image.Picture.Bitmap.Width  := Image.Width;
      Image.Picture.Bitmap.Height := Image.Height;
    end;

  procedure TForm1.RenderView;
    var
      Scale : single;

    function convX( i, j, z  : integer ) : integer;
      begin
        result := Image.Width div 2 + round(Scale*2*(i - j));
      end;

    function convY( i, j, z : integer ) : integer;
      begin
        result := 8*Image.Height div 10 - round(Scale*(i + j)) - z;
      end;

    var
      i, j   : integer;
      poly   : array[0..4] of TPoint;
      rgb    :
        record
          r, g, b, x : byte;
        end;
    begin
      Image.Picture.Bitmap.Canvas.Brush.Color := clBlack;
      Image.Picture.Bitmap.Canvas.Rectangle( 0, 0, Image.Width, Image.Height );
      Scale := round(2*Image.Width/(fCargoSystem.xSize*cos(pi/6) + fCargoSystem.ySize*cos(pi/6))) div 5;
      for i := fCargoSystem.ySize - 2 downto 0 do
        for j := 1 to fCargoSystem.xSize - 2 do
          begin
            poly[0].x := convX( i, j, fCargoSystem[0].Value[j, i] );
            poly[0].y := convY( i, j, fCargoSystem[0].Value[j, i] );
            poly[4].x := convX( i, j, fCargoSystem[0].Value[j, i] );
            poly[4].y := convY( i, j, fCargoSystem[0].Value[j, i] );
            poly[1].x := convX( i + 1, j, fCargoSystem[0].Value[j, i + 1] );
            poly[1].y := convY( i + 1, j, fCargoSystem[0].Value[j, i + 1] );
            poly[2].x := convX( i + 1, j + 1, fCargoSystem[0].Value[j + 1, i + 1] );
            poly[2].y := convY( i + 1, j + 1, fCargoSystem[0].Value[j + 1, i + 1] );
            poly[3].x := convX( i, j + 1, fCargoSystem[0].Value[j + 1, i] );
            poly[3].y := convY( i, j + 1, fCargoSystem[0].Value[j + 1, i] );
            rgb.r := 255 - min( 255, 2*(255*abs(fCargoSystem[0].Value[j, i])) div MaxVal );
            rgb.g := min( 255, 4*(255*abs(fCargoSystem[0].Value[j, i])) div MaxVal );
            rgb.b := min( 255, 2*(255*abs(fCargoSystem[0].Value[j, i])) div MaxVal );
            Image.Picture.Bitmap.Canvas.Brush.Color := TColor(rgb);
            rgb.r := min( 255, rgb.r*3 );
            rgb.g := min( 255, rgb.g*3 );
            rgb.b := min( 255, rgb.b*3 );
            Image.Picture.Bitmap.Canvas.Pen.Color := TColor(rgb);
            Image.Picture.Bitmap.Canvas.Polygon( poly );
          end;
    end;

  procedure TForm1.FormShow(Sender: TObject);
    begin
      RenderView;
    end;

  procedure TForm1.Panel1Resize(Sender: TObject);
    begin
      // RenderView;
    end;

  procedure TForm1.Panel2Click(Sender: TObject);
    begin
      if HighQuality.Checked
        then fCargoSystem.Render( rqHigh )
        else fCargoSystem.Render( rqLow );
      RenderView;
    end;

end.


