unit CabMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CabUtils, ComCtrls;

type
  TCabMainForm =
    class(TForm)
        Button1: TButton;
        OpenDialog1: TOpenDialog;
        Edit1: TEdit;
        Label1: TLabel;
        StatusBar1: TStatusBar;
        ProgressBar1: TProgressBar;
        procedure Button1Click(Sender: TObject);
      public
        procedure SetStatusText( const aText : string );
        procedure SetProgress( Percent : integer );
    end;

var
  CabMainForm: TCabMainForm;

implementation

{$R *.DFM}

  var
    DestDir : string;

  function DecompressNotify( UserId : pointer; fdint : TFDINotificationType; var pfdin : TFDINotification ) : integer; stdcall;
    begin
      case fdint of
        fdintCOPY_FILE:
          begin
            TCabMainForm(UserId).SetStatusText( 'Decompressing ' + pfdin.psz1 );
            Result := 0;
          end;

        fdintNEXT_CABINET : Result :=  -1; //abort

        fdintPROGRESS :
          begin
            TCabMainForm(UserId).SetProgress( pfdin.cb );
            Result := 0;
          end;

        fdintCLOSE_FILE_INFO :
          begin
            TCabMainForm(UserId).SetProgress( 0 );
            Result := 0;
          end;

        else Result := 0;
      end;
    end;

  procedure TCabMainForm.Button1Click(Sender: TObject);
    begin
      if OpenDialog1.Execute
        then
          begin
            DestDir := Edit1.Text;
            DecompressCabFiles( pchar(OpenDialog1.Filename), pchar(DestDir), self, DecompressNotify );
            SetStatusText( '' );
          end;
    end;

  procedure TCabMainForm.SetStatusText( const aText : string );
    begin
      StatusBar1.Panels[0].Text := aText;
      Application.ProcessMessages;
    end;

  procedure TCabMainForm.SetProgress( Percent : integer );
    begin
      ProgressBar1.Position := Percent;
      Application.ProcessMessages;
    end;

end.
