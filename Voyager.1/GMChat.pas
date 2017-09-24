unit GMChat;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, FramedButton, VoyagerInterfaces, VoyagerServerInterfaces;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    ChatText: TRichEdit;
    Panel3: TPanel;
    Panel4: TPanel;
    ChatLine: TMemo;
    btnConnect: TFramedButton;
  public
    fClientView       : IClientView;
    fMasterURLHandler : IMasterURLHandler;
  public
    property ClientView : IClientView write fClientView;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}





end.
