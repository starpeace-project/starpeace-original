unit SplashDesigner;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, SpeedImage, ComCtrls, ExtDlgs;

type
  TfmSplashDesigner =
    class( TForm )
    btOK: TButton;
    pcExpertContainer: TPageControl;
    tsExpertPage1: TTabSheet;
    llIntro: TLabel;
    tsExpertPage2: TTabSheet;
    paSplashPreview: TPanel;
    siSplashPreview: TSpeedImage;
    btBrowseSplashImage: TButton;
    odSplashImage: TOpenPictureDialog;
        btCancel      : TButton;
        bvSep1        : TBevel;
    gbSplashOptions: TGroupBox;
    cbUseTransparent: TCheckBox;
    cbUseRegistry: TCheckBox;
    cbStayOnTop: TCheckBox;

    procedure btOKClick(Sender: TObject);
    procedure btBrowseSplashImageClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);

      end;

var
  fmSplashDesigner : TfmSplashDesigner;

implementation

{$R *.DFM}

  uses
    Buffer, SpeedBmp;
    
  procedure TfmSplashDesigner.btOKClick(Sender: TObject);
    begin
      if pcExpertContainer.ActivePage = tsExpertPage1
        then pcExpertContainer.ActivePage := tsExpertPage2
        else {};
    end;

  procedure TfmSplashDesigner.btBrowseSplashImageClick(Sender: TObject);
    begin
      with odSplashImage do
        if Execute
          then siSplashPreview.Picture := LoadBitmapFile( Filename );
    end;

  procedure TfmSplashDesigner.btCancelClick(Sender: TObject);
    begin
      Close;
    end;


end.
