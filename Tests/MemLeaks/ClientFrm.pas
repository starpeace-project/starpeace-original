unit ClientFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, RDOInterfaces, MemoryManager;

type
  TMainClientForm = class(TForm)
    tTicks: TTimer;
    btnStart: TButton;
    MemoryManagerClass1: TMemoryManagerClass;
    MemoryManagerClass2: TMemoryManagerClass;
    Label1: TLabel;
    Label2: TLabel;
    procedure btnStartClick(Sender: TObject);
    procedure tTicksTimer(Sender: TObject);
  private
    fProxy : OleVariant;
    fCnnt  : IRDOConnectionInit;
  end;

var
  MainClientForm: TMainClientForm;

implementation

{$R *.DFM}

  uses
    WinsockRDOConnection, RDOObjectProxy;

  procedure TMainClientForm.btnStartClick(Sender: TObject);
    begin
      btnStart.Enabled := false;
      tTicks.Enabled := true;
      fCnnt  := TWinSockRDOConnection.Create('Test');
      fCnnt.Port := 5001;
      fCnnt.Server := 'localhost';
      if fCnnt.Connect(10000)
        then
          begin
            fProxy := TRDOObjectProxy.Create as IDispatch;
            fProxy.SetConnection(fCnnt);
            fProxy.BindTo('Server');
          end
        else Application.Terminate;
    end;

  procedure TMainClientForm.tTicksTimer(Sender: TObject);
    var
      str : string;
    begin
      str := fProxy.GetStr(random(10000));
    end;

end.
