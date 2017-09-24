unit MessageBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, FramedButton, InternationalizerComponent;

type
  TMsgBoxFrm = class(TForm)
    BorderPanel: TPanel;
    Panel2: TPanel;
    CaptionPanel: TPanel;
    TextPanel: TPanel;
    Text: TLabel;
    BtnsPanel: TPanel;
    bOK: TFramedButton;
    bCancel: TFramedButton;
    Panel1: TPanel;
    IconNotebook: TNotebook;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    InternationalizerComponent1: TInternationalizerComponent;
    procedure FormShow(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  function ShowMsgBox( Title, Body : string; Icon : integer; ShowOk, ShowCancel : boolean ) : TModalResult;

implementation
                                
{$R *.DFM}

  procedure TMsgBoxFrm.FormShow(Sender: TObject);
    begin
      TextPanel.Height := Text.Height;
      Height := TextPanel.Height + CaptionPanel.Height + TextPanel.Top + BtnsPanel.Height + 2*BorderPanel.BevelWidth + 25;
    end;

  function ShowMsgBox( Title, Body : string; Icon : integer; ShowOk, ShowCancel : boolean ) : TModalResult;
    var
      Box : TMsgBoxFrm;
    begin
      Box := TMsgBoxFrm.Create( nil );
      try
        try
          Box.CaptionPanel.Caption   := Title;     
          Box.Text.Caption           := Body;
          Box.IconNotebook.PageIndex := Icon;
          Box.bOK.Visible            := ShowOK;
          Box.bCancel.Visible        := ShowCancel;
          result := Box.ShowModal;
        except
          result := mrCancel;
        end;
      finally
        Box.Free;
      end;
    end;

  procedure TMsgBoxFrm.bOKClick(Sender: TObject);
    begin
      ModalResult := mrOk;
    end;

  procedure TMsgBoxFrm.bCancelClick(Sender: TObject);
    begin
      ModalResult := mrCancel;
    end;

end.
