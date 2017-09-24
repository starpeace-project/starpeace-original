unit TrainingDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, MarqueeCtrl, FramedButton,
  VoyagerInterfaces, VoyagerServerInterfaces;

type
  TTrainingDlg = class(TForm)
    CloseBtn: TFramedButton;
    HintText: TMarquee;
    Shape1: TShape;
    btCreate: TFramedButton;
    btCancel: TFramedButton;
    Label13: TLabel;
    procedure edTeamNameChange(Sender: TObject);
    procedure btCreateClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
  private
    fClientView       : IClientView;
    fMasterURLHandler : IMasterURLHandler;
    fIllSystem        : olevariant;
    fCriminalName     : string;
  public
    property ClientView       : IClientView       write fClientView;
    property MasterURLHandler : IMasterURLHandler write fMasterURLHandler;
    property IllSystem        : olevariant        write fIllSystem;
    property CriminalName     : string            write fCriminalName;
  private
    procedure threadedNewTeam( const parms : array of const );
    procedure syncNewTeam( const parms : array of const );
  end;

var
  TrainingDlg: TTrainingDlg;

implementation

  {$R *.DFM}
  
  uses
    Threads;

  procedure TTrainingDlg.edTeamNameChange(Sender: TObject);
    begin
      btCreate.Enabled := edTeamName.Text <> '';
      pnError.Visible  := false;
    end;

  procedure TTrainingDlg.btCreateClick(Sender: TObject);
    begin
      Fork( threadedNewTeam, priNormal, [edTeamName.Text] );
    end;

  procedure TTrainingDlg.btCancelClick(Sender: TObject);
    begin
      ModalResult := mrCancel;
    end;

  procedure TTrainingDlg.threadedNewTeam( const parms : array of const );
    var
      name : string absolute parms[0].vPChar;
    begin
      try
        if fIllSystem.RDOCreateTeam( fCriminalName, name ) <> 'no'
          then Join( syncNewTeam, [0] )
          else Join( syncNewTeam, [1] );
      except
      end;
    end;

  procedure TTrainingDlg.syncNewTeam( const parms : array of const );
    var
      ErrorCode : integer absolute parms[0].vInteger;
    begin
      case ErrorCode of
        0 : ModalResult := mrOk;
        else
          begin
            pnError.Caption := GetLiteral('Literal177');
            pnError.Visible := true;
          end;
      end;
    end;



end.



