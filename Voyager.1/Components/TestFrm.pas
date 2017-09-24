unit TestFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, PercentEdit, GradientBox, FingerTabs, ExtCtrls, MarqueeCtrl;

type
  TForm2 = class(TForm)
    eNewTab: TEdit;
    bAdd: TButton;
    Update: TButton;
    Button1: TButton;
    Button2: TButton;
    Perc: TLabel;
    Marquee: TMarquee;
    Timer1: TTimer;
    Timer2: TTimer;
    FingerTabs1: TFingerTabs;
    PercentEdit1: TPercentEdit;
    procedure UpdateClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure PercentEdit1Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure bAddClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    procedure WMEraseBkgnd(var Message: TMessage); //message WM_ERASEBKGND;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.DFM}

procedure TForm2.UpdateClick(Sender: TObject);
  begin
    Marquee.Caption := eNewTab.Text;
    //Tabs.EndUpdate;
  end;

procedure TForm2.Button2Click(Sender: TObject);
  begin
    Marquee.Digitalize := not Marquee.Digitalize;
  end;

procedure TForm2.PercentEdit1Change(Sender: TObject);
  begin
    //Perc.Caption := IntToStr(PercentEdit1.Value);
  end;

procedure TForm2.WMEraseBkgnd(var Message: TMessage);
  begin
    //Message.Result := 1;
  end;

procedure TForm2.Timer1Timer(Sender: TObject);
  begin
    Marquee.Tick;
  end;

var
  x : integer = 111;

procedure TForm2.Timer2Timer(Sender: TObject);
  begin
    Marquee.Caption := 'La pasta ' + IntToStr(x);
    inc(x);
  end;

procedure TForm2.bAddClick(Sender: TObject);
  begin
    //Timer2.Enabled := not Timer2.Enabled;
  end;

procedure TForm2.Button1Click(Sender: TObject);
  begin
    Marquee.ShowGrill := not Marquee.ShowGrill;
  end;

end.

