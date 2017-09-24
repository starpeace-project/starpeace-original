unit ChaseListDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TChaseListDlg = class(TForm)
    Shape1: TShape;
    UserList: TListBox;
    Label1: TLabel;
    procedure UserListDblClick(Sender: TObject);
    procedure UserListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ChaseListDlg: TChaseListDlg;

implementation

  {$R *.DFM}

  procedure TChaseListDlg.UserListDblClick(Sender: TObject);
    begin
      ModalResult := mrOk;
    end;

  procedure TChaseListDlg.UserListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    begin
      case Key of
        vk_Return :
          ModalResult := mrOk;
        vk_Escape :
          ModalResult := mrCancel;
      end;
    end;

end.
