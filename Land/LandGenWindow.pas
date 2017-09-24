unit LandGenWindow;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Buttons, LandGenerator, Land;

type
  TLandGenWin = class(TForm)
    Panel1: TPanel;
    ScrollBox1: TScrollBox;
    SrcBmp: TImage;
    OpenBmp: TOpenDialog;
    OpenClasses: TOpenDialog;
    LoadBitmap: TSpeedButton;
    SpeedButton1: TSpeedButton;
    Splitter1: TSplitter;
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
    procedure FormCreate(Sender: TObject);
    procedure LoadBitmapClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    fLandRenderer : TLandRenderer;
    LandImgs : array[TLandType] of TImage;
  end;

var
  LandGenWin: TLandGenWin;

implementation

  {$R *.DFM}

  procedure TLandGenWin.FormCreate(Sender: TObject);
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
          end
        else Close;
    end;

  procedure TLandGenWin.LoadBitmapClick(Sender: TObject);
    var
      filename : string;
    begin
      if OpenBmp.Execute and (OpenBmp.FileName <> '')
        then
          begin
            filename := OpenBmp.FileName;
            SrcBmp.Picture.LoadFromFile( filename );
            fLandRenderer.LoadLandInfo( filename );
          end;
    end;

  procedure TLandGenWin.SpeedButton1Click(Sender: TObject);
    const
      MaxRes = 4500;
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
      VisualClass : TVisualClass;
      pal : PLogPalette;
      hpal : HPALETTE;
      line : PByteArray;
      rgb : TRGB;
  begin
      fLandRenderer.RenderLand;
      DestBmp.Picture.Bitmap := TBitmap.Create;
      DestBmp.Picture.Bitmap.PixelFormat := pf8bit;
      GetMem(pal, sizeof(TLogPalette) + sizeof(TPaletteEntry) * 255);
      pal.palVersion := $300;
      pal.palNumEntries := 256;
      for i := 0 to pred(fLandRenderer.VisualClasses.Count) do
        with TVisualClass(fLandRenderer.VisualClasses[i]) do
          begin
            if LandTypeOf( LandVisualClassId ) <> ldtSpecial
              then rgb := TRGB(MapColor)
              else rgb := TRGB(fLandRenderer.fMainColors[LandClassOf( LandVisualClassId )]);
            pal.palPalEntry[LandVisualClassId].peRed := rgb.r;
            pal.palPalEntry[LandVisualClassId].peGreen := rgb.g;
            pal.palPalEntry[LandVisualClassId].peBlue := rgb.b;
          end;
      hpal := CreatePalette(pal^);
      if hpal <> 0 then
        DestBmp.Picture.Bitmap.Palette := hpal;
      DestBmp.Picture.Bitmap.Width := fLandRenderer.LandSize.x;
      DestBmp.Picture.Bitmap.Height := fLandRenderer.LandSize.y;
      for y := 0 to pred(fLandRenderer.LandSize.y) do
        begin
          line := DestBmp.Picture.Bitmap.ScanLine[y];
          for x := 0 to pred(fLandRenderer.LandSize.x) do
            line[x] := fLandRenderer.LandDest[y*fLandRenderer.LandSize.x + x];
        end;
      if SaveDialog.Execute and (SaveDialog.FileName <> '')
        then DestBmp.Picture.Bitmap.SaveToFile( SaveDialog.FileName );
      {
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
            LandType := LandTypeOf(fLandRenderer.LandDest[y*fLandRenderer.LandSize.x + x]);
            LandImg := LandImgs[LandType];
            DestRect := Bounds( landCenter.Width*x, landCenter.Height*y, landCenter.Width, landCenter.Height );
            if LandImg <> nil
              then DestBmp.Picture.Bitmap.Canvas.CopyRect( DestRect, LandImg.Picture.Bitmap.Canvas, SourceRect );
          end;
      }
    end;


end.
