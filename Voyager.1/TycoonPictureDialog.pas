unit TycoonPictureDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, TiledPanel, MultiBMPButton, StdCtrls;

type
  TTycoonPictureDlg = class(TForm)
    Panel1: TPanel;
    TiledPanel1: TTiledPanel;
    MultiBMPButton1: TMultiBMPButton;
    MultiBMPButton2: TMultiBMPButton;
    MultiBMPButton3: TMultiBMPButton;
    TiledPanel2: TTiledPanel;
    Panel2: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Image1: TImage;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  TycoonPictureDlg: TTycoonPictureDlg;

implementation

{$R *.DFM}



end.
