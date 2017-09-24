unit CGIReportModule;

interface

uses
  Windows, Messages, SysUtils, Classes, HTTPApp;

type
  TCGIWebModule = class(TWebModule)
    procedure CGIWebModuleGetReportAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CGIWebModule: TCGIWebModule;

implementation

  uses
    GIFImage, Graphics, ActiveX, ComObj;

{$R *.DFM}

function GetWorldData( const ISAddr : string; ISPort : integer; out WorldName : string; out WorldPop, UserCount, Year, OnlineUsers : integer ) : boolean; 
  var
    RDOConn : olevariant;
    Obj     : olevariant;
    x       : pointer;
  begin
    try
      x := nil;
      CoInitialize( x );
      try
        RDOConn := CreateOleObject( 'RDOClient.WinSockRDOConnection' );
        RDOConn.Server := ISAddr;
        RDOConn.Port := ISPort;
        if RDOConn.Connect( 5000 )
          then
            begin
              Obj := CreateOleObject( 'RDOClient.RDOObjectProxy' );
              Obj.SetConnection( RDOConn );
              if Obj.BindTo( 'InterfaceServer' )
                then
                  begin
                    WorldName   := Obj.WorldName;
                    WorldPop    := Obj.WorldPopulation;
                    UserCount   := Obj.UserCount;
                    Year        := Obj.WorldYear;
                    OnlineUsers := 12;
                    result := WorldPop >= 0;
                  end
                else result := false;
            end
          else result := false
      finally
        CoUninitialize;
      end;
    except
      result := false;
    end
  end;

procedure TCGIWebModule.CGIWebModuleGetReportAction(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  type
    TPalette = (palLight, palDark);
    TColorItem = (itmHeader, itmNormal);
  const
    Style : array[TPalette, TColorItem] of TColor =
      (($0000AAFF, $00FFFFFF),
       ($000044AA, clGray));
  const
    ImgWidth  = 150;
    ImgHeight = 80;
  var
    Palette     : TPalette;
    Bitmap      : TBitmap;
    GIFImage    : TGIFImage;
    Ext         : TGIFGraphicControlExtension;
    Stream      : TMemoryStream;
    WorldName   : string;
    WorldPop    : integer;
    UserCount   : integer;
    Year        : integer;
    OnlineUsers : integer;
  begin
    try
      Response.StatusCode := 200;
      Response.LastModified := Now;
      Response.Expires := 0;
      Response.Date := Now;
      Response.ContentType := 'image/gif';
      Response.Title := 'Star Peace Server Status';
      // Create BMP
      Bitmap := TBitmap.Create;
      try
        Bitmap.PixelFormat := pf16bit;
        Bitmap.Width       := ImgWidth;
        Bitmap.Height      := ImgHeight;
        Bitmap.Canvas.Brush.Color := clBlack;
        Bitmap.Canvas.Rectangle( 0, 0, Bitmap.Width, Bitmap.Height );
        Bitmap.Canvas.Pen.Style := psSolid;
        if uppercase(Request.QueryFields.Values['Style']) = 'DARK'
          then Palette := palDark
          else Palette := palLight;
        if (Request.QueryFields.Values['Addr'] <> '') and (Request.QueryFields.Values['Port'] <> '') and
           GetWorldData( Request.QueryFields.Values['Addr'], StrToInt(Request.QueryFields.Values['Port']), WorldName, WorldPop, UserCount, Year, OnlineUsers )
           then
             begin
                Bitmap.Canvas.Font.Color := Style[Palette, itmHeader];
                Bitmap.Canvas.Font.Name := 'Tahoma';
                Bitmap.Canvas.Font.Style := [fsBold];
                Bitmap.Canvas.Font.Size  := 10;
                Bitmap.Canvas.TextOut( 1, 1, WorldName );
                Bitmap.Canvas.Font.Color := Style[Palette, itmNormal];
                Bitmap.Canvas.Font.Style := [];
                Bitmap.Canvas.Font.Size  := 8;
                Bitmap.Canvas.TextOut( 1, 20, Format('%.0n', [WorldPop/1]) + ' inhabitants' );
                Bitmap.Canvas.TextOut( 1, 35, Format('%.0n', [UserCount/1]) + ' tycoons' );
                Bitmap.Canvas.TextOut( 1, 50, 'year ' + IntToStr(Year) );
                // Bitmap.Canvas.TextOut( 1, 65, IntToStr(OnlineUsers) + ' online' );
             end
           else
             begin
                Bitmap.Canvas.Font.Color := Style[Palette, itmNormal];
                Bitmap.Canvas.Font.Style := [fsBold];
                Bitmap.Canvas.Font.Size  := 10;
                Bitmap.Canvas.TextOut( 1, 1, 'Server not found' );
                Bitmap.Canvas.Font.Color := Style[Palette, itmNormal];
                Bitmap.Canvas.Font.Style := [];
                Bitmap.Canvas.Font.Size  := 8;
                Bitmap.Canvas.TextOut( 1, 15, '(Star Peace is down.)' );
                Bitmap.Height := 40;
             end;
        // create GIF
        GIFImage := TGIFImage.Create;
        try
          GIFImage.BackgroundColor := clBlack;
          GIFImage.Assign( Bitmap );
          Ext := TGIFGraphicControlExtension.Create(GIFImage.Images[0]);
          Ext.Transparent := True;
          Ext.TransparentColorIndex := GIFImage.Images[0].Pixels[0, 0];
          GIFImage.Images[0].Extensions.Add(Ext);
          // Create and return stream
          Stream := TMemoryStream.Create;
          try
            GIFImage.SaveToStream( Stream );
            Stream.Position := 0;
            Response.ContentLength := Stream.Size;
            Response.ContentStream := Stream;
          except
            Stream.Free;
            raise;
          end;
        finally
          GIFImage.Free;
        end;
      finally
        Bitmap.Free;
      end;
      Handled := true;
    except
      Response.StatusCode := 500;
      Handled := false;
    end;
  end;

end.
