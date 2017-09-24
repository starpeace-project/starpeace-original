unit NewTeamDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, MarqueeCtrl, FramedButton,
  VoyagerInterfaces, VoyagerServerInterfaces, InternationalizerComponent;

type
  TNewTeamDlg = class(TForm)
    CloseBtn: TFramedButton;
    HintText: TMarquee;
    Shape1: TShape;
    Label4: TLabel;
    edTeamName: TEdit;
    btCreate: TFramedButton;
    pnError: TPanel;
    btCancel: TFramedButton;
    InternationalizerComponent1: TInternationalizerComponent;
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
  NewTeamDlg: TNewTeamDlg;

implementation

  {$R *.DFM}
  
  uses
    Threads, CrimeProtocol;

  procedure TNewTeamDlg.edTeamNameChange(Sender: TObject);
    begin
      btCreate.Enabled := edTeamName.Text <> '';
      pnError.Visible  := false;
    end;

  procedure TNewTeamDlg.btCreateClick(Sender: TObject);
    begin
      Fork( threadedNewTeam, priNormal, [edTeamName.Text] );
    end;

  procedure TNewTeamDlg.btCancelClick(Sender: TObject);
    begin
      ModalResult := mrCancel;
    end;

  procedure TNewTeamDlg.threadedNewTeam( const parms : array of const );
    var
      name : string absolute parms[0].vPChar;
      ErrorCode : TErrorCode;
    begin
      try
        ErrorCode := fIllSystem.RDOCreateTeam( fCriminalName, name );
        Join( syncNewTeam, [ErrorCode] )
      except
      end;
    end;

  procedure TNewTeamDlg.syncNewTeam( const parms : array of const );
    var
      ErrorCode : integer absolute parms[0].vInteger;
    begin
      case ErrorCode of
        CRIME_NOERROR :
          ModalResult := mrOk;
        CRIME_ERROR_WrongTeamName :
          begin
            pnError.Caption := 'Error: Invalid team name';
            pnError.Visible := true;
          end;
        else
          begin
            pnError.Caption := 'Unknown error. Please try again.';
            pnError.Visible := true;
          end;
      end;
    end;


end.



