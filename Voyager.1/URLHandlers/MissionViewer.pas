unit MissionViewer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, FramedButton, ExtCtrls, ComCtrls, InternationalizerComponent;

type
  TMisionView = class(TForm)
    Label2: TLabel;
    Label3: TLabel;
    Notebook1: TNotebook;
    CloseBtn: TFramedButton;
    ListBox1: TListBox;
    Label13: TLabel;
    FramedButton6: TFramedButton;
    FramedButton1: TFramedButton;
    Label1: TLabel;
    FramedButton2: TFramedButton;
    FramedButton3: TFramedButton;
    FramedButton4: TFramedButton;
    Label4: TLabel;
    ListView1: TListView;
    MissionOption1: TCheckBox;
    MissionWays1: TComboBox;
    Label5: TLabel;
    Label9: TLabel;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    CheckBox2: TCheckBox;
    ComboBox2: TComboBox;
    CheckBox3: TCheckBox;
    ComboBox3: TComboBox;
    FramedButton5: TFramedButton;
    FramedButton7: TFramedButton;
    FramedButton8: TFramedButton;
    Label6: TLabel;
    Panel1: TPanel;
    Memo1: TMemo;
    InternationalizerComponent1: TInternationalizerComponent;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MisionView: TMisionView;

implementation

{$R *.DFM}


end.
