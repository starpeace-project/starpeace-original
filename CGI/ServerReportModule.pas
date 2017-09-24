unit ServerReportModule;

interface

uses
  ShareMem, Windows, Messages, SysUtils, Classes, HTTPApp, Graphics, GIFImage;

type
  TServerReportWebModule = class(TWebModule)
    procedure ServerReportWebModuleGetReportAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ServerReportWebModule: TServerReportWebModule;

implementation                                                            

{$R *.DFM}

// function GetWorldData( const ISAddr : string; ISPort : integer; out WorldName : string; out WorldPop, UserCount, Year, OnlineUsers : integer ) : boolean; external 'ISInterface.dll';
{
function GetWorldData( const ISAddr : string; ISPort : integer; out WorldName : string; out WorldPop, UserCount, Year, OnlineUsers : integer ) : boolean;
  begin
    result := false;
  end;
}

procedure TServerReportWebModule.ServerReportWebModuleGetReportAction( Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  {
  const
    ImgWidth  = 150;
    ImgHeight = 80;
  var
    Bitmap      : TBitmap;
    GIFImage    : TGIFImage;
    Ext         : TGIFGraphicControlExtension;
    Stream      : TMemoryStream;
    WorldName   : string;
    WorldPop    : integer;
    UserCount   : integer;
    Year        : integer;
    OnlineUsers : integer;
    }
  begin
    Response.Content := 'HELLO YOU!';
    Handled := true;
    (*
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
        if {(Request.QueryFields.Values['Addr'] <> '') and (Request.QueryFields.Values['Port'] <> '') and}
           GetWorldData( 'lisa', 8000, WorldName, WorldPop, UserCount, Year, OnlineUsers )
           //GetWorldData( Request.QueryFields.Values['Addr'], StrToInt(Request.QueryFields.Values['Port']), WorldName, WorldPop, UserCount, Year, OnlineUsers )
           then
             begin
                Bitmap.Canvas.Font.Color := $000088FF;
                Bitmap.Canvas.Font.Name := 'Tahoma';
                Bitmap.Canvas.Font.Style := [fsBold];
                Bitmap.Canvas.Font.Size  := 10;
                Bitmap.Canvas.TextOut( 1, 1, WorldName );
                Bitmap.Canvas.Font.Color := clWhite;
                Bitmap.Canvas.Font.Style := [];
                Bitmap.Canvas.Font.Size  := 8;
                Bitmap.Canvas.TextOut( 1, 20, IntToStr(WorldPop) + ' inhabitants' );
                Bitmap.Canvas.TextOut( 1, 35, IntToStr(UserCount) +' tycoons' );
                Bitmap.Canvas.TextOut( 1, 50, 'year ' + IntToStr(Year) );
                Bitmap.Canvas.TextOut( 1, 65, IntToStr(OnlineUsers) + ' online' );
             end
           else
             begin
                Bitmap.Canvas.Font.Color := clWhite;
                Bitmap.Canvas.Font.Style := [];
                Bitmap.Canvas.Font.Size  := 8;
                Bitmap.Canvas.TextOut( 1, 1, 'Sorry, could not establish' );
                Bitmap.Canvas.TextOut( 1, 12, 'connection with server.' );
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
    *)
  end;

end.
