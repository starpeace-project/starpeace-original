unit HistoryDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, MarqueeCtrl, FramedButton,
  VoyagerInterfaces, VoyagerServerInterfaces, InternationalizerComponent;

type
  THistoryDlg = class(TForm)
    CloseBtn: TFramedButton;
    HintText: TMarquee;
    Shape1: TShape;
    History: TListBox;
    FramedButton1: TFramedButton;
    InternationalizerComponent1: TInternationalizerComponent;
    procedure btCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FramedButton1Click(Sender: TObject);
  private
    fClientView       : IClientView;
    fMasterURLHandler : IMasterURLHandler;
    fIllSystem        : olevariant;
    fCriminal         : TStringList;
    fLeader           : TStringList;
    fTeam             : TStringList;
  public
    property ClientView       : IClientView       write fClientView;
    property MasterURLHandler : IMasterURLHandler write fMasterURLHandler;
    property IllSystem        : olevariant        write fIllSystem;
    property Criminal         : TStringList       write fCriminal;
    property Leader           : TStringList       write fLeader;
    property Team             : TStringList       write fTeam;
  private
    procedure threadedGetHistory( const parms : array of const );
    procedure syncGetHistory( const parms : array of const );
  end;

var
  HistoryDlg: THistoryDlg;

implementation

  {$R *.DFM}
  
  uses
    Threads, Literals, CoolSB;

  procedure THistoryDlg.btCancelClick(Sender: TObject);
    begin
      ModalResult := mrCancel;
    end;

  procedure THistoryDlg.threadedGetHistory( const parms : array of const );
    var
      Info : TStringList;
    begin
      Info := TStringList.Create;
      try
        Info.Text := fIllSystem.RDORecoveryHistoryItem( '', '', fCriminal.Values['Name'] );
        Join( syncGetHistory, [Info] );
      except
        Info.Free;
      end;
    end;

  procedure THistoryDlg.syncGetHistory( const parms : array of const );
    var
      Info  : TStringList absolute parms[0].vPointer;
      count : integer;
      i     : integer;
    begin
      try
        History.Items.Clear;
        count := StrToInt(Info.Values['ItemNumber']);
        for i := 0 to count do // >> change to pred(count) when server is fixed!
          History.Items.Add( GetFormattedLiteral('Literal175', [Info.Values['Item' + IntToStr(i) + '-' + 'Date'] + ' - ' + Info.Values['Item' + IntToStr(i) + '-' + 'Event']] ));
      finally
        Info.Free;
      end;
    end;

  procedure THistoryDlg.FormShow(Sender: TObject);
    begin
      if InitSkinImage
        then
          begin
            InitializeCoolSB(History.Handle);
            if hThemeLib <> 0
              then
                SetWindowTheme(History.Handle, ' ', ' ');
          end;
      History.Items.Clear;
      History.Items.Add( GetLiteral('Literal176') );
      Fork( threadedGetHistory, priNormal, [0] );
    end;

  procedure THistoryDlg.FramedButton1Click(Sender: TObject);
    begin
      ModalResult := mrCancel;
    end;

end.



