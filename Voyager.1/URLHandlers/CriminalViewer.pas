unit CriminalViewer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FramedButton, GradientBox, StdCtrls, ExtCtrls, PercentEdit, MarqueeCtrl,
  VoyagerInterfaces, VoyagerServerInterfaces,
  ColoredGauge, CrimeMainViewer, InternationalizerComponent;

const
  SkillSlots = 12;

type
  TCriminalView = class(TForm)
    lbName: TLabel;
    GradientBox1: TGradientBox;
    btFire: TFramedButton;
    btTrain: TFramedButton;
    btMove: TFramedButton;
    btHistory: TFramedButton;
    SalaryBar: TPercentEdit;
    lbSalary: TLabel;
    SalaryValue: TLabel;
    lbRole: TLabel;
    cbRole: TComboBox;
    Panel3: TPanel;
    Panel4: TPanel;
    StatusMarquee: TMarquee;
    Image: TImage;
    SkillName5: TLabel;
    SkillBar5: TColorGauge;
    SkillVal5: TLabel;
    SkillName6: TLabel;
    SkillVal6: TLabel;
    SkillBar6: TColorGauge;
    SkillVal7: TLabel;
    SkillBar7: TColorGauge;
    SkillName8: TLabel;
    SkillVal8: TLabel;
    SkillBar8: TColorGauge;
    SkillName1: TLabel;
    SkillVal1: TLabel;
    SkillBar1: TColorGauge;
    SkillName2: TLabel;
    SkillVal2: TLabel;
    SkillBar2: TColorGauge;
    SkillName3: TLabel;
    SkillVal3: TLabel;
    SkillBar3: TColorGauge;
    CloseBtn: TFramedButton;
    SkillName9: TLabel;
    SkillVal9: TLabel;
    SkillBar9: TColorGauge;
    SkillName10: TLabel;
    SkillBar10: TColorGauge;
    SkillVal10: TLabel;
    SkillName11: TLabel;
    SkillVal11: TLabel;
    SkillBar11: TColorGauge;
    SkillName12: TLabel;
    SkillVal12: TLabel;
    SkillBar12: TColorGauge;
    SkillName7: TLabel;
    SkillName4: TLabel;
    SkillVal4: TLabel;
    SkillBar4: TColorGauge;
    StatusTimer: TTimer;
    InternationalizerComponent1: TInternationalizerComponent;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure btFireClick(Sender: TObject);
    procedure cbRoleChange(Sender: TObject);
    procedure btHistoryClick(Sender: TObject);
    procedure btTrainClick(Sender: TObject);
    procedure SalaryBarChange(Sender: TObject);
    procedure StatusTimerTimer(Sender: TObject);
  private
    SkillNames  : array[0..SkillSlots - 1] of TLabel;
    SkillValues : array[0..SkillSlots - 1] of TLabel;
    SkillBars   : array[0..SkillSlots - 1] of TColorGauge;
  private
    fClientView       : IClientView;
    fMasterURLHandler : IMasterURLHandler;
    fIllSystem        : olevariant;
    fProperties       : TStringList;
    fLeader           : TStringList;
    fTeam             : TStringList;
    fTeamMember       : boolean;
    fOnTeamModified   : TOnTeamModified;
    fMainView         : TCrimeMainView;
    fSourceImg        : TBitmap;
    fBltScan          : integer;
    fLastPict         : string;
  private
    procedure SetProperties( Prop : TStringList );
  public
    property ClientView       : IClientView       write fClientView;
    property MasterURLHandler : IMasterURLHandler write fMasterURLHandler;
    property IllSystem        : olevariant        write fIllSystem;
    property Properties       : TStringList       write SetProperties;
    property Leader           : TStringList       write fLeader;
    property Team             : TStringList       write fTeam;
    property TeamMember       : boolean           read fTeamMember write fTeamMember;
    property OnTeamModified   : TOnTeamModified   write fOnTeamModified;
    property MainView         : TCrimeMainView    write fMainView;
  private
    procedure threadedFire( const parms : array of const );
    procedure syncFire( const parms : array of const );
  end;

var
  CriminalView: TCriminalView;

implementation

{$R *.DFM}

  uses
    MetaCrime, ClassStorage, Events, JPGtoBMP, Threads, HistoryDialog, CrimeTrainingDialog, CrimeProtocol;

  procedure TCriminalView.SetProperties( Prop : TStringList );
    var
      i     : integer;
      MA    : TMetaAttribute;
      cache : string;
      State : TCriminalState;
    begin
      try
        fProperties.Free;
        fProperties := TStringList.Create;
        fProperties.Assign( Prop );
        for i := 0 to pred(TheClassStorage.ClassCount['Attribute']) do
          begin
            MA := TMetaAttribute(TheClassStorage.ClassByIdx['Attribute', i]);
            if i < SkillSlots
              then
                begin
                  SkillNames[i].Caption  := MA.Name;
                  SkillNames[i].Hint     := MA.Desc;
                  SkillValues[i].Caption := Prop.Values[IntToStr(i)] + '%';
                  SkillBars[i].Position  := StrToInt(Prop.Values[IntToStr(i)]);
                  SkillNames[i].Visible  := true;
                  SkillValues[i].Visible := true;
                  SkillBars[i].Visible   := true;
                end;
          end;
        for i := TheClassStorage.ClassCount['Attribute'] to pred(SkillSlots) do
          begin
            SkillNames[i].Visible  := false;
            SkillValues[i].Visible := false;
            SkillBars[i].Visible   := false;
          end;
        cbRole.Items.Clear;
        for i := 0 to pred(TheClassStorage.ClassCount['Role']) do
          begin
            cbRole.Items.Add( TMetaRole(TheClassStorage.ClassByIdx['Role', i]).Name );
          end;
        cbRole.ItemIndex := cbRole.Items.IndexOf( fProperties.Values['Role'] );
        //cbRole.ItemIndex := cbRole.Items.IndexOf(fTeam.Values['Role'+ IntToStr(fTeam.IndexOf())]);
        {
        for i := 1 to 8 do
          begin
            cbRole.Items.Clear;

          end;
        }
        fMasterURLHandler.HandleEvent( evnAnswerPrivateCache, cache );
        if fProperties.Values['Picture'] <> fLastPict
          then
            try
              Image.Picture.Bitmap := TBitmap.Create;
              Image.Picture.Bitmap.PixelFormat := pf24bit;
              Image.Picture.Bitmap.Width := Image.Width;
              Image.Picture.Bitmap.Height := Image.Height;
              with Image.Picture.Bitmap.Canvas do
                begin
                  Pen.Style   := psClear;
                  Brush.Color := $00004080;
                  Rectangle( 0, 0, Image.Picture.Bitmap.Width + 1, Image.Picture.Bitmap.Height + 1 );
                end;
              TVFilter( Image.Picture.Bitmap, $00243940 );
              fSourceImg.Free;
              fSourceImg := TBitmap.Create;
              fSourceImg.PixelFormat := pf24bit;
              fSourceImg.Width := Image.Width;
              fSourceImg.Height := Image.Height;
              LoadJPGToBMP( cache + tidPath_IBImages + fProperties.Values['Picture'] + '.jpg', fSourceImg );
              TVFilter( fSourceImg, $00243940 );
              fBltScan := 1;
              fLastPict := fProperties.Values['Picture'];
            except
            end;
        State := TCriminalState(StrToInt(fProperties.Values['State']));
        StatusMarquee.ForeColor := StateColors[State];
        case State of
          tcsInTeamTraining :
            StatusMarquee.Caption := StateLabels[State] + ': ' + fProperties.Values['TrainingClass'];
          else
            StatusMarquee.Caption := StateLabels[State];
        end;
        SalaryValue.Caption := fProperties.Values['SalaryPerc'] + '% ($' + IntToStr(round(StrToFloat(fProperties.Values['Salary']))) + '/h)';
        SalaryBar.Value := StrToInt(fProperties.Values['SalaryPerc']);
        lbName.Caption := fProperties.Values['Name'];
        btTrain.Enabled := State in [tcsNoState, tcsOnTheMarket, tcsOnTheMarketHidden, tcsInTeamStandBy];
        SalaryBar.Visible := fTeamMember;
        SalaryValue.Visible := fTeamMember;
        lbSalary.Visible := fTeamMember;
        btFire.Visible := fTeamMember;
        btTrain.Visible := fTeamMember;
        btMove.Visible := fTeamMember;
        //btHistory.Visible := fTeamMember;
        cbRole.Visible := fTeamMember;
        lbRole.Visible := fTeamMember;
      except
      end;
    end;

  procedure TCriminalView.FormCreate(Sender: TObject);
    begin
      SkillNames[0]   := SkillName1;
      SkillNames[1]   := SkillName2;
      SkillNames[2]   := SkillName3;
      SkillNames[3]   := SkillName4;
      SkillNames[4]   := SkillName5;
      SkillNames[5]   := SkillName6;
      SkillNames[6]   := SkillName7;
      SkillNames[7]   := SkillName8;
      SkillNames[8]   := SkillName9;
      SkillNames[9]   := SkillName10;
      SkillNames[10]  := SkillName11;
      SkillNames[11]  := SkillName12;
      SkillValues[0]  := SkillVal1;
      SkillValues[1]  := SkillVal2;
      SkillValues[2]  := SkillVal3;
      SkillValues[3]  := SkillVal4;
      SkillValues[4]  := SkillVal5;
      SkillValues[5]  := SkillVal6;
      SkillValues[6]  := SkillVal7;
      SkillValues[7]  := SkillVal8;
      SkillValues[8]  := SkillVal9;
      SkillValues[9]  := SkillVal10;
      SkillValues[10] := SkillVal11;
      SkillValues[11] := SkillVal12;
      SkillBars[0]    := SkillBar1;
      SkillBars[1]    := SkillBar2;
      SkillBars[2]    := SkillBar3;
      SkillBars[3]    := SkillBar4;
      SkillBars[4]    := SkillBar5;
      SkillBars[5]    := SkillBar6;
      SkillBars[6]    := SkillBar7;
      SkillBars[7]    := SkillBar8;
      SkillBars[8]    := SkillBar9;
      SkillBars[9]    := SkillBar10;
      SkillBars[10]   := SkillBar11;
      SkillBars[11]   := SkillBar12;
    end;

  procedure TCriminalView.FormShow(Sender: TObject);
    begin
      Top := 3;
      Left := 400;
      if fTeam <> nil
        then
          begin
            SalaryBar.MidValue := 100;
          end;
    end;

  procedure TCriminalView.CloseBtnClick(Sender: TObject);
    begin
      Close;
    end;

  procedure TCriminalView.btFireClick(Sender: TObject);
    begin
      Fork( threadedFire, priNormal, [fProperties.Values['Name'], fTeam.Values['Name'], fLeader.Values['Name']] );
    end;

  procedure TCriminalView.threadedFire( const parms : array of const );
    var
      name, team, leader : string;
      ErrorCode          : TErrorCode;
    begin
      try
        name   := parms[0].vPChar;
        team   := parms[1].vPChar;
        leader := parms[2].vPChar;
        ErrorCode := fIllSystem.RDOFireCriminal( leader, team, name );
        Join( syncFire, [name, ErrorCode] );
      except
      end;
    end;

  procedure TCriminalView.syncFire( const parms : array of const );
    var
      name : string absolute parms[0].vPChar;
    begin
      if assigned(fOnTeamModified)
        then fOnTeamModified;
      if fProperties.Values['Name'] = name
        then Close;
    end;

  procedure TCriminalView.cbRoleChange(Sender: TObject);
    var
      ErrorCode : TErrorCode;
    begin
      try
        ErrorCode := fIllSystem.RDOChangeRole( fLeader.Values['Name'], fTeam.Values['Name'], fProperties.Values['Name'], cbRole.Text );
        if (ErrorCode = CRIME_NOERROR) and assigned(fOnTeamModified)
          then fOnTeamModified;
      except
      end;
    end;

  procedure TCriminalView.btHistoryClick(Sender: TObject);
    begin
      HistoryDlg.Team := fTeam;
      HistoryDlg.Leader := fLeader;
      HistoryDlg.Criminal := fProperties;
      HistoryDlg.IllSystem := fIllSystem;
      HistoryDlg.MasterURLHandler := fMasterURLHandler;
      HistoryDlg.ClientView := fClientView;
      HistoryDlg.ShowModal;
    end;

  procedure TCriminalView.btTrainClick(Sender: TObject);
    begin
      CrimeTrainingDlg.Team := fTeam;
      CrimeTrainingDlg.Leader := fLeader;
      CrimeTrainingDlg.Criminal := fProperties;
      CrimeTrainingDlg.IllSystem := fIllSystem;
      CrimeTrainingDlg.MasterURLHandler := fMasterURLHandler;
      CrimeTrainingDlg.ClientView := fClientView;
      if (CrimeTrainingDlg.ShowModal = mrOk) and assigned(fOnTeamModified)
        then fOnTeamModified;
    end;

  procedure TCriminalView.SalaryBarChange(Sender: TObject);
    begin
      SalaryValue.Caption := IntToStr(SalaryBar.Value) + '%';
    end;

  procedure TCriminalView.StatusTimerTimer(Sender: TObject);
    const
      Step = 10;
    var
      R : TRect;
    begin
      StatusMarquee.Tick;
      if fBltScan > 0
        then
          if fBltScan + Step <= fSourceImg.Height
            then
              begin
                R := Rect(0, 0, fSourceImg.Width, fBltScan);
                Image.Picture.Bitmap.Canvas.CopyRect( R, fSourceImg.Canvas, R );
                with Image.Picture.Bitmap.Canvas do
                  begin
                    Pen.Color := clGray;
                    Brush.Color := clGray;
                    Rectangle( 0, fBltScan - 2, Image.Picture.Bitmap.Width, fBltScan - 1 );
                    Pen.Color := clSilver;
                    Brush.Color := clSilver;
                    Rectangle( 0, fBltScan - 1, Image.Picture.Bitmap.Width, fBltScan );
                    Pen.Color := clWhite;
                    Brush.Color := clWhite;
                    Rectangle( 0, fBltScan, Image.Picture.Bitmap.Width, fBltScan + 1 );
                  end;
                inc( fBltScan, Step );
              end
            else
              begin
                R := Rect(0, 0, fSourceImg.Width, fSourceImg.Height);
                Image.Picture.Bitmap.Canvas.CopyRect( R, fSourceImg.Canvas, R );
                fBltScan := 0;
              end;
    end;

end.



