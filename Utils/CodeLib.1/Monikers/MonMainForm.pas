unit MonMainForm;

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls;

  type
    TMonikersMainForm =
      class(TForm)
          Button1: TButton;
          Edit1: TEdit;
          Label1: TLabel;
          StatusBar1: TStatusBar;
          procedure Button1Click(Sender: TObject);
        private
          function  DownLoadNotify( Progress: Longint; ProgressMax : Longint; StatusCode: Longint; StatusText: PWideChar ) : HRESULT;
          procedure SetStatusText( aText : string );
      end;

  var
    MonikersMainForm: TMonikersMainForm;

implementation

  uses
    URLUtils, URLMon;

{$R *.DFM}

  procedure TMonikersMainForm.Button1Click(Sender: TObject);
    var
      FName : string;
    begin
      FName := DownloadURLToCacheFile( Edit1.Text, DownLoadNotify );
      SetStatusText( FName );
    end;

  function TMonikersMainForm.DownLoadNotify( Progress: Longint; ProgressMax : Longint; StatusCode: Longint; StatusText: PWideChar ) : HRESULT;
    begin
      case StatusCode of
        BINDSTATUS_FINDINGRESOURCE :
          begin
            SetStatusText( 'Finding Resource : ' + StatusText );
          end;

        BINDSTATUS_CONNECTING :
          begin
            SetStatusText( 'Connecting to : ' + StatusText );
          end;

        BINDSTATUS_REDIRECTING :
          begin
            SetStatusText( 'Redirecting to : ' + StatusText );
          end;

        BINDSTATUS_BEGINDOWNLOADDATA :
          begin
            SetStatusText( 'Downloading : ' + StatusText + ' ' + IntToStr( trunc( (Progress/ProgressMax)*100 )));
          end;

        BINDSTATUS_DOWNLOADINGDATA :
          begin
            SetStatusText( 'Downloading : ' + StatusText + ' ' + IntToStr( trunc( (Progress/ProgressMax)*100 )));
          end;

        BINDSTATUS_ENDDOWNLOADDATA :
          begin
            SetStatusText( '' );
          end;

        BINDSTATUS_BEGINDOWNLOADCOMPONENTS :
          begin
            SetStatusText( 'BEGINDOWNLOADCOMPONENTS' );
          end;

        BINDSTATUS_INSTALLINGCOMPONENTS :
          begin
            SetStatusText( 'INSTALLINGCOMPONENTS' );
          end;

        BINDSTATUS_ENDDOWNLOADCOMPONENTS :
          begin
            SetStatusText( 'ENDDOWNLOADCOMPONENTS' );
          end;

        BINDSTATUS_USINGCACHEDCOPY :
          begin
            SetStatusText( 'We will use cahed copy' );
          end;

        BINDSTATUS_SENDINGREQUEST :
          begin
          end;

        BINDSTATUS_CLASSIDAVAILABLE :
          begin
          end;

        BINDSTATUS_MIMETYPEAVAILABLE :
          begin
          end;

        BINDSTATUS_CACHEFILENAMEAVAILABLE :
          begin
            SetStatusText( StatusText );
          end;
      end;
      Result := S_OK;
    end;

  procedure TMonikersMainForm.SetStatusText( aText : string );
    begin
      StatusBar1.Panels[0].Text := aText;
      Application.ProcessMessages;
      sleep( 1000 );
    end;

end.
