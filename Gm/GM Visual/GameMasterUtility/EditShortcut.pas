unit EditShortcut;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, TextShortcuts, InternationalizerComponent;

type
  TEditShortcutForm =
    class(TForm)
        DescEdit : TEdit;
        Label1   : TLabel;
        Label2   : TLabel;
        HotKey1  : THotKey;
        Label3   : TLabel;
        TextEdit : TEdit;
        Bevel1   : TBevel;
        Button1  : TButton;
        Button2  : TButton;
    InternationalizerComponent1: TInternationalizerComponent;
        procedure Button1Click(Sender: TObject);
        procedure Button2Click(Sender: TObject);
      public
        procedure setShortcutManager( aTextShortcutMger : TTextShortcutMger );
      private
        fTextShortcutMger : TTextShortcutMger;
        fEditing          : boolean;
        procedure setEditing( value : boolean );
      public
        property Editing : boolean write setEditing;
    end;

var
  EditShortcutForm: TEditShortcutForm;

implementation

  {$R *.DFM}

  uses
    Literals;

  procedure TEditShortcutForm.setShortcutManager( aTextShortcutMger : TTextShortcutMger );
    begin
      fTextShortcutMger := aTextShortcutMger;
    end;

  procedure TEditShortcutForm.setEditing( value : boolean );
    begin
      fEditing := value;
      if fEditing
        then Button1.Caption := 'Ok'//GetLiteral('Literal447')
        else Button1.Caption := 'Add';//GetLiteral('Literal448');
    end;

  procedure TEditShortcutForm.Button1Click(Sender: TObject);
    begin
      if not fEditing
        then
          begin
            if fTextShortcutMger.getShortcutByKey( HotKey1.HotKey ) <> nil
              then ShowMessage( 'There are conflicts!!'{GetLiteral('Literal449')} )
              else ModalResult := mrOK;
          end
        else ModalResult := mrOK;
    end;

  procedure TEditShortcutForm.Button2Click(Sender: TObject);
    begin
      ModalResult := mrCancel;
    end;

end.
