unit MapViewerWindow;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Buttons, LandGenerator, Land;

type
  TLandViewerWin = class(TForm)
    Panel1: TPanel;
    OpenBmp: TOpenDialog;
    OpenClasses: TOpenDialog;
    LoadBitmap: TSpeedButton;
    ScrollBox2: TScrollBox;
    DestBmp: TImage;
    landN: TImage;
    LandS: TImage;
    LandE: TImage;
    LandW: TImage;
    LandNEo: TImage;
    landSEo: TImage;
    landSWo: TImage;
    landNWo: TImage;
    landCenter: TImage;
    landNWi: TImage;
    landNEi: TImage;
    landSEi: TImage;
    landSWi: TImage;
    landSpecial: TImage;
    SaveDialog: TSaveDialog;
    SpeedButton1: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure LoadBitmapClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SaveMask(Sender: TObject);
  private
    fLandRenderer : TLandRenderer;
    LandImgs : array[TLandType] of TImage;
  end;

var
  LandViewerWin: TLandViewerWin;

implementation

  {$R *.DFM}

  procedure TLandViewerWin.FormCreate(Sender: TObject);
    var
      LandType : TLandType;
    begin
      OpenClasses.InitialDir := ExtractFilePath( paramstr(0) );
      if OpenClasses.Execute and (OpenClasses.FileName <> '')
        then
          begin
            fLandRenderer := TLandRenderer.Create( ExtractFilePath(OpenClasses.FileName) );
            Application.MessageBox( pchar(IntToStr(fLandRenderer.ClassCount) + ' classes read.'), 'Done', MB_ICONINFORMATION or MB_OK );
            LandImgs[ldtCenter]  := landCenter;
            LandImgs[ldtN]       := landN;
            LandImgs[ldtE]       := landE;
            LandImgs[ldtS]       := landS;
            LandImgs[ldtW]       := landW;
            LandImgs[ldtNEo]     := landNEo;
            LandImgs[ldtSEo]     := landSEo;
            LandImgs[ldtSWo]     := landSWo;
            LandImgs[ldtNWo]     := landNWo;
            LandImgs[ldtNEi]     := landNEi;
            LandImgs[ldtSEi]     := landSEi;
            LandImgs[ldtSWi]     := landSWi;
            LandImgs[ldtNWi]     := landNWi;
            LandImgs[ldtSpecial] := landSpecial;
            {
            for LandType := low(LandType) to high(LandType) do
              with LandImgs[LandType] do
                begin
                  Stretch := true;
                  Width   := Width div 2;
                  Height  := Height div 2;
                end;
            }
          end
        else Close;
    end;

  procedure TLandViewerWin.LoadBitmapClick(Sender: TObject);
    var
      filename : string;
    begin
      if OpenBmp.Execute and (OpenBmp.FileName <> '')
        then
          begin
            filename := OpenBmp.FileName;
            fLandRenderer.LoadRenderedLand( filename );
            SpeedButton1Click( self );
          end;
    end;

  procedure TLandViewerWin.SpeedButton1Click(Sender: TObject);
    const
      MaxRes = 4000;
    type
      TRGB =
        packed record
          r, g, b, x : byte;
        end;
    var
      x, y, i : integer;
      SourceRect, DestRect : TRect;
      LandType : TLandType;
      LandImg : TImage;
  begin
      DestBmp.Picture.Bitmap := TBitmap.Create;
      if landCenter.Width*fLandRenderer.LandSize.x < MaxRes
        then DestBmp.Picture.Bitmap.Width := landCenter.Width*fLandRenderer.LandSize.x
        else DestBmp.Picture.Bitmap.Width := MaxRes;
      if landCenter.Height*fLandRenderer.LandSize.y < MaxRes
        then DestBmp.Picture.Bitmap.Height := landCenter.Height*fLandRenderer.LandSize.y
        else DestBmp.Picture.Bitmap.Height := MaxRes;
      SourceRect := Bounds( 0, 0, landCenter.Width, landCenter.Height );
      for x := 0 to pred(fLandRenderer.LandSize.x) do
        for y := 0 to pred(fLandRenderer.LandSize.y) do
          begin
            LandType := LandTypeOf(fLandRenderer.LandSource[y*fLandRenderer.LandSize.x + x]);
            LandImg := LandImgs[LandType];
            DestRect := Bounds( landCenter.Width*x, landCenter.Height*y, landCenter.Width, landCenter.Height );
            if LandImg <> nil
              then DestBmp.Picture.Bitmap.Canvas.CopyRect( DestRect, LandImg.Picture.Bitmap.Canvas, SourceRect );
          end;
    end;
    
  procedure TLandViewerWin.SaveMask(Sender: TObject);
    var
      Mask : TBitmap;
    begin
      Mask := TBitmap.Create;
    end;

end.
