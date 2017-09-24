unit ShortcutForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, TextShortcuts,
  StdCtrls, ComCtrls, Menus, InternationalizerComponent;

type
  TShortcutCenter =
    class(TForm)
        ListView1: TListView;
        Label1: TLabel;
        Button1: TButton;
        Button2: TButton;
        Button3: TButton;
        Button4: TButton;
        InternationalizerComponent1: TInternationalizerComponent;
        procedure Button1Click(Sender: TObject);
        procedure Button2Click(Sender: TObject);
        procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
      public
        procedure setShortcutManager( aTextShortcutMger : TTextShortcutMger );
      private
        fTextShortcutMger : TTextShortcutMger;
        procedure FillList;
        procedure Clear;
        procedure AddItem( Desc, Text : string; Shortcut : TShortcut; aData : pointer );

    end;

var
  ShortcutCenter: TShortcutCenter;

implementation

  uses
    EditShortcut;

{$R *.DFM}

  // TShortcutCenter

  procedure TShortcutCenter.setShortcutManager( aTextShortcutMger : TTextShortcutMger );
    begin
      fTextShortcutMger := aTextShortcutMger;
      EditShortcutForm.setShortcutManager( aTextShortcutMger );
      Clear;
      FillList;
    end;

  procedure TShortcutCenter.FillList;
    var
      i : integer;
    begin
      for i := 0 to pred(fTextShortcutMger.ShortcutCount) do
        AddItem( fTextShortcutMger.Shortcuts[i].Desc, fTextShortcutMger.Shortcuts[i].Text, fTextShortcutMger.Shortcuts[i].Shortcut, fTextShortcutMger.Shortcuts[i] );
    end;

  procedure TShortcutCenter.Clear;
    begin
      ListView1.Items.Clear;
    end;

  procedure TShortcutCenter.AddItem( Desc, Text : string; Shortcut : TShortcut; aData : pointer );
    begin
      with ListView1.Items.Add do
        begin
          Caption     := Desc;
          SubItems.Add( ShortCutToText(Shortcut));
          SubItems.Add( Text);
          Data        := aData;
        end;
    end;

  procedure TShortcutCenter.Button1Click(Sender: TObject);
    var
      shotcutInfo : TShortcutInfo;
    begin
      with EditShortcutForm do
        begin
          Editing       := false;
          DescEdit.Text := '';
          TextEdit.Text := '';
        end;
      if EditShortcutForm.ShowModal = mrOK
        then
          begin
            shotcutInfo := fTextShortcutMger.AddShortcutText( EditShortcutForm.HotKey1.HotKey, EditShortcutForm.DescEdit.Text, EditShortcutForm.TextEdit.Text );
            AddItem( shotcutInfo.Desc, shotcutInfo.Text, shotcutInfo.Shortcut, shotcutInfo );
          end;
    end;

  procedure TShortcutCenter.Button2Click(Sender: TObject);
    var
      shotcutInfo : TShortcutInfo;
      Item        : TListItem;
    begin
      Item := ListView1.Selected;
      if Item <> nil
        then
          begin
            shotcutInfo := TShortcutInfo(Item.Data);
            with EditShortcutForm do
              begin
                Editing        := true;
                DescEdit.Text  := shotcutInfo.Desc;
                TextEdit.Text  := shotcutInfo.Text;
                HotKey1.HotKey := shotcutInfo.Shortcut;
              end;
            if EditShortcutForm.ShowModal = mrOK
              then
                begin
                  with EditShortcutForm do
                    begin
                      shotcutInfo.Desc := DescEdit.Text;
                      shotcutInfo.Text := TextEdit.Text;
                      shotcutInfo.Shortcut := HotKey1.HotKey;

                      Item.Caption     := shotcutInfo.Desc;
                      Item.SubItems[0] := ShortCutToText(shotcutInfo.Shortcut);
                      Item.SubItems[1] := shotcutInfo.Text;
                    end;
                end;
          end;
    end;

procedure TShortcutCenter.Button4Click(Sender: TObject);
begin
  Close;
end;

procedure TShortcutCenter.Button3Click(Sender: TObject);
var
  Shortcut : TShortcut;
  ItemSelected : TListItem;
begin
  ItemSelected := ListView1.ItemFocused;
  if ItemSelected <> nil
    then
      begin
        Shortcut := TextToShortcut( ItemSelected.SubItems[0] ) ;
        fTextShortcutMger.DeleteShortcutText( Shortcut );
        ListView1.Items.Delete( ListView1.Items.IndexOf( ItemSelected ) );
      end;
end;

end.
