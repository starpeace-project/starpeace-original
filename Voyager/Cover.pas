unit Cover;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ColoredGauge, Synchro, InternationalizerComponent;

type
  TCoverForm = class(TForm)
    CoverPanel: TPanel;
    Backround: TImage;
    StatusPanel: TPanel;
    OverallProg: TColorGauge;
    Comment: TLabel;
    InternationalizerComponent1: TInternationalizerComponent;
    WorldName: TLabel;
    procedure CoverPanelResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormHide(Sender: TObject);
  private
    fCacheDir  : string;
    fCancelled : boolean;
  public
    property CacheDir : string read fCacheDir write fCacheDir;
  public
    procedure OnSyncNotify( SyncTask : TSyncTask; EventId  : TSyncEventId; TaskDesc : string; Progress, OverallProgress : integer; out Cancel : boolean );
  end;

var
  CoverForm: TCoverForm;

implementation

{$R *.DFM}

  uses
    jpeg, Literals, WinBtns;

  procedure TCoverForm.CoverPanelResize(Sender: TObject);
    begin
      StatusPanel.Top := Height - StatusPanel.Height;
    end;
                                                                                                              
  procedure TCoverForm.FormShow(Sender: TObject);
    var
      jpg : TJPEGImage;
      tmp : TBitmap;
      i   : integer;
    begin
      jpg := TJPEGImage.Create;
      try
        jpg.LoadFromFile( CacheDir + 'otherimages\logonportal.jpg' );
        tmp := TBitmap.Create;
        try
          tmp.Assign( jpg );
          Backround.Picture.Bitmap := TBitmap.Create;
          Backround.Picture.Bitmap.Width  := Backround.Width;
          Backround.Picture.Bitmap.Height := Backround.Height;
          StretchBlt(
            Backround.Picture.Bitmap.Canvas.Handle, 0, 0, Backround.Picture.Bitmap.Width, Backround.Picture.Bitmap.Height,
            tmp.Canvas.Handle, 0, 0, tmp.Width, tmp.Height, SRCCOPY );
          {
          with Backround.Picture.Bitmap.Canvas do
            begin
              Pen.Color := clBlack;
              Pen.Style := psSolid;
              for i := 0 to pred(Backround.Picture.Bitmap.Height) div 2 do
                begin
                  MoveTo( 0, 2*i );
                  LineTo( Backround.Picture.Bitmap.Width, 2*i );
                end;
            end;
          }
        finally
          tmp.Free;
        end;
      finally
        jpg.Free;
      end;
    end;

  procedure TCoverForm.OnSyncNotify( SyncTask : TSyncTask; EventId : TSyncEventId; TaskDesc : string; Progress, OverallProgress : integer; out Cancel : boolean );
    begin
      Comment.Caption := GetLiteral('Literal26');
      OverallProg.Position := OverallProgress;
      Application.ProcessMessages;
      Cancel := fCancelled;
    end;

procedure TCoverForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  begin
    CanClose := true;
    fCancelled := true;
  end;

procedure TCoverForm.FormClose(Sender: TObject; var Action: TCloseAction);
  begin
    try
      Backround.Picture.Bitmap.FreeImage;
    except
    end;
    Action := caFree;
  end;

procedure TCoverForm.FormHide(Sender: TObject);
  begin
    if WinBtnsView<>nil
      then WinBtnsView.Hide;
  end;

end.
