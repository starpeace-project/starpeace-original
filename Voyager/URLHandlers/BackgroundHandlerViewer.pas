unit BackgroundHandlerViewer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  VisualControls, ExtCtrls, InternationalizerComponent;

type
  TBackgroundHandlerView = class(TVisualControl)
    Panel1: TPanel;
    Image: TImage;
    InternationalizerComponent1: TInternationalizerComponent;
    procedure Panel1Resize(Sender: TObject);
  private
    fCache : string;
  public
    property Cache : string read fCache write fCache;
  end;

var
  BackgroundHandlerView: TBackgroundHandlerView;

implementation

{$R *.DFM}

  uses
    JPGtoBMP;

  procedure TBackgroundHandlerView.Panel1Resize(Sender: TObject);
    const
      Ratio = 800/600;
    begin
      Image.Height := Height;
      Image.Width  := round(Height*Ratio);
      Image.Picture.Bitmap := TBitmap.Create;
      Image.Picture.Bitmap.Width := Image.Width;
      Image.Picture.Bitmap.Height := Image.Height;
      LoadJPGToBMP( fCache + 'OtherImages\tempscreen.jpg', Image.Picture.Bitmap );
      TVFilter( Image.Picture.Bitmap, clBlack );
    end;

end.
