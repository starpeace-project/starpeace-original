unit SyncTest;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, Synchro;

type
  TForm1 = class(TForm)
    Source: TEdit;
    Destination: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    ProgressBar: TProgressBar;
    Status: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    procedure Notify( SyncTask : TSyncTask; EventId : TSyncEventId; TaskDesc : string; Progress : integer );
  end;

var
  Form1: TForm1;

implementation

  uses
    URLUtils;

  {$R *.DFM}

  procedure TForm1.Button1Click(Sender: TObject);
    begin
      AsyncSynchronize( Source.Text, Destination.Text, Notify, 0 );
    end;

  procedure TForm1.Notify( SyncTask : TSyncTask; EventId : TSyncEventId; TaskDesc : string; Progress : integer );
    begin
      Status.Caption := TaskDesc;
      ProgressBar.Position := Progress;
      Application.ProcessMessages;
    end;
    
end.
