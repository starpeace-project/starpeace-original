unit VoicePanelViewer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  VisualControls, ExtCtrls, FramedButton, ColoredGauge, VoyagerInterfaces, VoyagerServerInterfaces,
  StdCtrls, InternationalizerComponent;

type
  TVoicePanel = class(TVisualControl)
    ColorGauge1: TColorGauge;
    Panel1: TPanel;
    Talk: TFramedButton;
    VUGauge: TColorGauge;
    BufferGauge: TColorGauge;
    TimeLimit: TColorGauge;
    OnTheAirSign: TNotebook;
    Image1: TImage;
    Image2: TImage;
    PendingSign: TNotebook;
    Image3: TImage;
    Image4: TImage;
    Label2: TLabel;
    Speaker: TLabel;                                          
    Phone: TImage;
    InternationalizerComponent1: TInternationalizerComponent;
    procedure TalkMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TalkMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PhoneClick(Sender: TObject);
  private
    fClientView       : IClientView;
    fMasterURLHandler : IMasterURLHandler;
  public
    property ClientView       : IClientView       write fClientView;
    property MasterURLHandler : IMasterURLHandler write fMasterURLHandler;
  end;

var
  VoicePanel: TVoicePanel;

implementation

{$R *.DFM}

  uses
    VoiceHandler;

  procedure TVoicePanel.TalkMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    begin
      fMasterURLHandler.HandleEvent( evnStartOfVoiceRecording, self );
    end;

  procedure TVoicePanel.TalkMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    begin
      fMasterURLHandler.HandleEvent( evnEndOfVoiceRecording, self );
    end;

  procedure TVoicePanel.PhoneClick(Sender: TObject);
    begin
      fMasterURLHandler.HandleURL( '?frame_Id=VoiceHandler&frame_Close=yes' );
    end;

end.
