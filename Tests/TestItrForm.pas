unit TestItrForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    ListBox: TListBox;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

  uses
    ComObj;

{$R *.DFM}

  procedure TForm1.Button1Click(Sender: TObject);
    var
      Iterator : OleVariant;
    begin
      Iterator := CreateOleObject( 'FolderIterator.DirectoryIterator' );
      Iterator.SetFolder( 'c:\inetpub\wwwroot\five\cache\Classes\PublicFacilities\', '*.*', 48 );
      repeat
        ListBox.Items.Add( Iterator.Current );
      until not Iterator.Next;
    end;

end.
