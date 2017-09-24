unit SimTst2Form;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Image1: TImage;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

  {$R *.DFM}

  uses
    SimTst2Blks;

  procedure TForm1.FormCreate(Sender: TObject);
    begin
      InitBlocks;
    end;

  procedure TForm1.Button1Click(Sender: TObject);
    var
      i : integer;
      t1, t2 : TDateTime;
    begin
      Asim := 0;
      Bsim := 0;
      Label1.Caption := 'Testing...';
      Application.ProcessMessages;
      t1 := Time;
      for i := 1 to 10 do
        SimBlocks;
      t2 := Time; 
      Label1.Caption := IntToStr( DateTimeToTimeStamp(t2 - t1).Time div 1000 ) + ' secs, ' + IntToStr( Asim + Bsim ) + ' iterations.';
    end;


end.
