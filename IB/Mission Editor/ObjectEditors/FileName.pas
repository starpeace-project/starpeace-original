unit FileName;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, EditableObjects, Editors;

type
  TFilenameEditor =
    class(TInPlaceVisualControl)
        Panel1: TPanel;
        Button1: TButton;
        Panel2: TPanel;
        edValue: TEdit;
    OpenDialog1: TOpenDialog;
        procedure Panel2Resize(Sender: TObject);
        procedure Panel1Resize(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure edValueChange(Sender: TObject);
      public
        procedure UpdateObject; override;
      protected
        procedure setEditableObject( aEditableObject : TEditableObject; options : integer ); override;
    end;

implementation

{$R *.DFM}

  procedure TFilenameEditor.Panel2Resize(Sender: TObject);
    begin
      edValue.SetBounds( 0, 0, Panel2.Width, Panel2.Height );
    end;

  procedure TFilenameEditor.Panel1Resize(Sender: TObject);
    begin
      Button1.SetBounds( 2, 2, Panel1.Width - 4, Panel1.Height - 4);
    end;

  procedure TFilenameEditor.UpdateObject;
    begin
      fEditableObject.Value := edValue.Text;
    end;

  procedure TFilenameEditor.setEditableObject( aEditableObject : TEditableObject; options : integer );
    function Translate( str : string ) : string;
      var
        p       : integer;
        appPath : string;
      begin
        p := pos( '{APP}', str );
        if p <> 0
          then
            begin
              appPath := ExtractFilePath( Application.ExeName );
              Delete( str, p, length('{APP}\') );
              Insert( appPath, str, p );
            end;
        result := str;
      end;
    var
      obj : TEditableObject;
    begin
      inherited;
      edValue.Text := fEditableObject.Value;
      obj          := fEditableObject.getProperty( 'ext' );
      if obj <> nil
        then OpenDialog1.Filter := obj.Value;

      obj := fEditableObject.getProperty( 'path' );
      if obj <> nil
        then OpenDialog1.InitialDir := Translate(obj.Value);
    end;

  procedure TFilenameEditor.Button1Click(Sender: TObject);
    begin
      if OpenDialog1.Execute
        then edValue.Text := OpenDialog1.FileName;
    end;

  procedure TFilenameEditor.edValueChange(Sender: TObject);
    begin
      UpdateObject;
    end;

end.
