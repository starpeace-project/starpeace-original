unit FGetClasses;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    ListBox1: TListBox;
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
var
   myClassName: Array[0..63] of Char;
   Handle: THandle;
begin
   ListBox1.Items.Clear;
   ListBox1.Font.Name := 'Courier';
   ListBox1.Items.Add(Format('%-7s %-64s',[' Handle','Class Name']));
   ListBox1.Items.Add(Format('%-7s %-64s',[' ------','----------']));
   {start off the list with the current application}
   Handle := Self.Handle;
   GetClassName(Self.Handle, myClassName, 64);
   ListBox1.Items.Add(Format('%7d %-64s',[Handle,StrPas(myClassName)]));
   {now list all the others}
   While Handle > 0 do
     begin
       Handle := GetNextWindow(Handle, GW_HWNDNEXT);
       GetClassName(Handle, myClassName, 64);
       if myClassName[0] <> '#' then
         ListBox1.Items.Add(
           Format('%7d %-64s',[Handle,StrPas(myClassName)]));
     end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
//
Button1Click ( Button1 ) ;
end;

end.
