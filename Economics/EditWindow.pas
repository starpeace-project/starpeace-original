unit EditWindow;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TEditWin = class(TForm)
    Panel1: TPanel;
    Modify: TSpeedButton;
    Cancel: TSpeedButton;
    Memo: TMemo;
    SaveAs: TSpeedButton;
    SaveDialog: TSaveDialog;
    procedure ModifyClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
    procedure SaveAsClick(Sender: TObject);
  private
    fFileName : string;
    procedure SetFileName( aFileName : string );
  public
    property FileName : string read fFileName write SetFileName;
  end;

var
  EditWin: TEditWin;

implementation

  {$R *.DFM}

  procedure TEditWin.ModifyClick(Sender: TObject);
    begin
      Memo.Lines.SaveToFile( fFileName );
      ModalResult := mrOk;
    end;

  procedure TEditWin.CancelClick(Sender: TObject);
    begin
      ModalResult := mrCancel;
    end;

  procedure TEditWin.SetFileName( aFileName : string );
    begin
      fFileName := aFileName;
      Caption := 'Editing [' + ExtractFileName(fFileName) + ']';
      Memo.Lines.LoadFromFile( fFileName );
    end;

  procedure TEditWin.SaveAsClick(Sender: TObject);
    begin
      if (SaveDialog.Execute) and (SaveDialog.FileName <> '')
        then
          begin
            Memo.Lines.SaveToFile( SaveDialog.FileName );
            ModalResult := mrOk;
          end
    end;

end.
