unit HireCriminalDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, IBSystem, IBTestMain;

type
  THireCriminal = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    lbExpert: TListBox;
    lbRookies: TListBox;
    Label3: TLabel;
    lbProperties: TListBox;
    Button1: TButton;
    Button2: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lbExpertClick(Sender: TObject);
    procedure lbRookiesClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    fCriminals : TStringList;
    procedure ShowSelected;
  public
    theIBSystem : TIBSystem;
    theTycoonId : integer;
    theTeamName : string;
    Selected    : TClientCriminal;
  end;

var
  HireCriminal: THireCriminal;

implementation

{$R *.DFM}

  procedure THireCriminal.FormShow(Sender: TObject);
    var
      str : string;
      i   : integer;
    begin
      lbExpert.Clear;
      lbRookies.Clear;
      lbProperties.Clear;
      fCriminals.Clear;

      str             := theIBSystem.RDOGetCriminalList( theTycoonId,  theTeamName );
      fCriminals.Text := str;

      if str <> ''
        then
          begin
            i := 0;
            while (fCriminals.Values[EXPERT_PREFIX + 'Name' + IntToStr(i)] <> '') do
              begin
                lbExpert.Items.add( fCriminals.Values[EXPERT_PREFIX + 'Name' + IntToStr(i)] );
                inc( i );
              end;

            i := 0;

            while (fCriminals.Values[ROOKIE_PREFIX + 'Name' + IntToStr(i)] <> '') do
              begin
                lbRookies.Items.add( fCriminals.Values[ROOKIE_PREFIX + 'Name' + IntToStr(i)] );
                inc( i );
              end;
          end
        else
          begin
            ShowMessage( 'unknow error getting the criminals list' );
            ModalResult := mrCancel;
          end;
    end;

  procedure THireCriminal.FormCreate(Sender: TObject);
    begin
      fCriminals := TStringList.Create;
      Selected   := TClientCriminal.Create;
    end;

  procedure THireCriminal.lbExpertClick(Sender: TObject);
    begin
      if lbExpert.ItemIndex <> -1
        then
          begin
            Selected.DeSerialize( fCriminals, EXPERT_PREFIX, lbExpert.ItemIndex );
            ShowSelected;
          end;
    end;

  procedure THireCriminal.lbRookiesClick(Sender: TObject);
    begin
      if lbRookies.ItemIndex <> -1
        then
          begin
            Selected.DeSerialize( fCriminals, ROOKIE_PREFIX, lbRookies.ItemIndex );
            ShowSelected;
          end;
    end;

  procedure THireCriminal.Button2Click(Sender: TObject);
    begin
      if (lbRookies.ItemIndex <> -1) or (lbExpert.ItemIndex <> -1)
        then ModalResult := mrOK
        else ShowMessage( 'Select a criminal to hire' );
    end;

  procedure THireCriminal.ShowSelected;
    begin
      lbProperties.Clear;
      lbProperties.Items.Add( 'Sex     : ' + Selected.Sex );
      lbProperties.Items.Add( 'Picture : ' + Selected.Picture );
      lbProperties.Items.Add( 'Status  : ' + Selected.Status );
      lbProperties.Items.Add( 'Skills:' );
      lbProperties.Items.Add( 'Leadership : ' + IntToStr( Selected.Skills[SKILL_LEADERSHIP] ) + '%' );
      lbProperties.Items.Add( 'Driving    : ' + IntToStr( Selected.Skills[SKILL_DRIVING] ) + '%' );
      lbProperties.Items.Add( 'Brawling   : ' + IntToStr( Selected.Skills[SKILL_BRAWLING] ) + '%' );
      lbProperties.Items.Add( 'Firearms   : ' + IntToStr( Selected.Skills[SKILL_FIREARMS] ) + '%' );
      lbProperties.Items.Add( 'Stalking   : ' + IntToStr( Selected.Skills[SKILL_STALKING] ) + '%' );
      lbProperties.Items.Add( 'Computer   : ' + IntToStr( Selected.Skills[SKILL_COMPUTER] ) + '%' );
      lbProperties.Items.Add( 'Demolition : ' + IntToStr( Selected.Skills[SKILL_DEMOLITION] ) + '%' );
      lbProperties.Items.Add( 'Stealth    : ' + IntToStr( Selected.Skills[SKILL_STEALTH] ) + '%' );
      lbProperties.Items.Add( 'Medicine   : ' + IntToStr( Selected.Skills[SKILL_MEDICINE] ) + '%' );
      lbProperties.Items.Add( 'Forgery    : ' + IntToStr( Selected.Skills[SKILL_FORGERY] ) + '%' );
    end;

  procedure THireCriminal.Button1Click(Sender: TObject);
    begin
      ModalResult := mrCancel;
    end;

end.
