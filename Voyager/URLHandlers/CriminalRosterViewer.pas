unit CriminalRosterViewer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  VoyagerInterfaces, VoyagerServerInterfaces,
  StdCtrls, ExtCtrls, FramedButton, CriminalViewer, CrimeMainViewer,
  InternationalizerComponent;

const
  ActiveSlots = 4;
  SocketColor = $00343924;

type
  TCriminalRosterView = class(TForm)
    Slot1: TPanel;
    Panel22: TPanel;
    Img1: TImage;
    Label11: TLabel;
    Slot2: TPanel;
    Panel5: TPanel;
    Img2: TImage;
    Slot3: TPanel;
    Panel4: TPanel;
    Img3: TImage;
    btPrev: TFramedButton;
    btMore: TFramedButton;
    btHire: TFramedButton;
    Slot4: TPanel;
    Panel6: TPanel;
    Img4: TImage;
    CloseBtn: TFramedButton;
    InternationalizerComponent1: TInternationalizerComponent;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SlotClick(Sender: TObject);
    procedure btMoreClick(Sender: TObject);
    procedure btPrevClick(Sender: TObject);
    procedure btHireClick(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
  private
    fClientView       : IClientView;
    fMasterURLHandler : IMasterURLHandler;
    fIllSystem        : olevariant;
    fLeader           : TStringList;
    fTeam             : TStringList;
    fOnTeamModified   : TOnTeamModified;
    fCriminals        : TStringList;
    fOffset           : integer;
    fCache            : string;
  private
    fImgs    : array[0..ActiveSlots - 1] of TImage;
    fSlots   : array[0..ActiveSlots - 1] of TPanel;
    fActSlot : integer;
  public
    property ClientView       : IClientView       write fClientView;
    property MasterURLHandler : IMasterURLHandler write fMasterURLHandler;
    property IllSystem        : olevariant        write fIllSystem;
    property Leader           : TStringList       write fLeader;
    property Team             : TStringList       write fTeam;
    property OnTeamModified   : TOnTeamModified   write fOnTeamModified;
  private
    procedure threadedGetCriminals( const parms : array of const );
    procedure syncAddCriminal( const parms : array of const );
    procedure threadedHireCriminal( const parms : array of const );
    procedure syncHireCriminal( const parms : array of const );
  private
    procedure RenderSlots;
  end;

var
  CriminalRosterView: TCriminalRosterView;

implementation

  {$R *.DFM}

  uses
    Threads, Events, JPGtoBMP, CrimeProtocol;

  procedure TCriminalRosterView.FormShow(Sender: TObject);
    var
      i : integer;
    begin
      if fCriminals <> nil
        then
          begin
            for i := 0 to pred(fCriminals.Count) do
              fCriminals.Objects[i].Free;
            fCriminals.Free;
          end;
      fMasterURLHandler.HandleEvent( evnAnswerPrivateCache, fCache );
      fCriminals := TStringList.Create;
      Fork( threadedGetCriminals, priNormal, [fLeader.Values['Name']] );
    end;

  procedure TCriminalRosterView.threadedGetCriminals( const parms : array of const );
    var
      name         : string absolute parms[0].vPchar;
      CriminalList : TStringList;
      CriminalProp : TStringList;
      i            : integer;
    begin
      CriminalList := TStringList.Create;
      try
        CriminalList.Text := fIllSystem.RDOGetCriminalNames( name );
        for i := 0 to pred(CriminalList.Count) do
          begin
            CriminalProp := TStringList.Create;
            try
              CriminalProp.Text := fIllSystem.RDOFindCriminal( CriminalList.Values[IntToStr(i + 1)] );
              Join( syncAddCriminal, [CriminalProp] );
            except
              CriminalProp.Free;
            end;
          end;
        Join( syncAddCriminal, [nil] );
      finally
        CriminalList.Free;
      end;
    end;

  procedure TCriminalRosterView.syncAddCriminal( const parms : array of const );
    var
      PropList : TStringList absolute parms[0].vPointer;
      idx      : integer;
    begin
      try
        if PropList <> nil
          then
            begin
              fCriminals.AddObject( PropList.Values['Name'], PropList );
              if (fCriminals.Count - fOffset <= ActiveSlots) and (fCriminals.Count - fOffset > 0)
                then
                  try
                    idx := fCriminals.Count - fOffset - 1;
                    fImgs[idx].Picture.Bitmap := TBitmap.Create;
                    fImgs[idx].Picture.Bitmap.PixelFormat := pf24bit;
                    fImgs[idx].Picture.Bitmap.Width := fImgs[idx].Width;
                    fImgs[idx].Picture.Bitmap.Height := fImgs[idx].Height;
                    LoadJPGToBMP( fCache + tidPath_IBImages + PropList.Values['Picture'] + '.jpg', fImgs[idx].Picture.Bitmap );
                    //TVFilter( fImgs[idx].Picture.Bitmap );
                    fSlots[idx].Enabled := true;
                  except
                  end;
              btPrev.Enabled := fOffset > 0;
              btMore.Enabled := fOffset + ActiveSlots < fCriminals.Count;
          end
        else
          begin
            for idx := fCriminals.Count - fOffset to ActiveSlots - 1 do
              begin
                fImgs[idx].Picture.Bitmap := TBitmap.Create;
                fImgs[idx].Picture.Bitmap.PixelFormat := pf24bit;
                fImgs[idx].Picture.Bitmap.Width := fImgs[idx].Width;
                fImgs[idx].Picture.Bitmap.Height := fImgs[idx].Height;
                LoadJPGToBMP( fCache + tidPath_IBImages + 'notavail.jpg', fImgs[idx].Picture.Bitmap );
                //TVFilter( fImgs[idx].Picture.Bitmap );
                fSlots[idx].Enabled := false;
              end;
          end;
      except
      end;
    end;

  procedure TCriminalRosterView.threadedHireCriminal( const parms : array of const );
    var
      name      : string absolute parms[0].vPChar;
      leader    : string;
      team      : string;
      ErrorCode : TErrorCode;
    begin
      try
        leader := parms[1].vPChar;
        team := parms[2].vPChar;
        ErrorCode := fIllSystem.RDOHireCriminal( leader, team, name, 100 );
        Join( syncHireCriminal, [name, ErrorCode] );
      except
      end;
    end;

  procedure TCriminalRosterView.syncHireCriminal( const parms : array of const );
    begin
      if assigned(fOnTeamModified)
        then fOnTeamModified;
      Close;
    end;

  procedure TCriminalRosterView.FormCreate(Sender: TObject);
    begin
      fImgs[0]  := Img1;
      fImgs[1]  := Img2;
      fImgs[2]  := Img3;
      fImgs[3]  := Img4;
      fSlots[0] := Slot1;
      fSlots[1] := Slot2;
      fSlots[2] := Slot3;
      fSlots[3] := Slot4;
      fActSlot  := -1;
    end;

  procedure TCriminalRosterView.SlotClick(Sender: TObject);
    var
      idx : integer;
    begin
      idx := TComponent(Sender).Tag;
      if (idx <> fActSlot) and fSlots[idx].Enabled
        then
          begin
            if fActSlot <> -1
              then fSlots[fActSlot].Color := SocketColor;
            fSlots[idx].Color := clLime;
            fActSlot          := idx;
            CriminalView.ClientView       := fClientView;
            CriminalView.MasterURLHandler := fMasterURLHandler;
            CriminalView.IllSystem        := fIllSystem;
            CriminalView.TeamMember       := false;
            CriminalView.Properties       := TStringList(fCriminals.Objects[fOffset + idx]);
            CriminalView.Show;
          end;
    end;

  procedure TCriminalRosterView.RenderSlots;
    var
      i   : integer;
      idx : integer;
    begin
      for i := 0 to pred(ActiveSlots) do
        if i + fOffset < fCriminals.Count
          then
            begin
              idx := i + fOffset;
              fImgs[i].Picture.Bitmap := TBitmap.Create;
              fImgs[i].Picture.Bitmap.PixelFormat := pf24bit;
              fImgs[i].Picture.Bitmap.Width := fImgs[i].Width;
              fImgs[i].Picture.Bitmap.Height := fImgs[i].Height;
              LoadJPGToBMP( fCache + tidPath_IBImages + TStringList(fCriminals.Objects[idx]).Values['Picture'] + '.jpg', fImgs[i].Picture.Bitmap );
              fSlots[i].Enabled := true;
              //TVFilter( fImgs[i].Picture.Bitmap );
            end
          else
            begin
              LoadJPGToBMP( fCache + tidPath_IBImages + 'notavail.jpg', fImgs[i].Picture.Bitmap );
              fSlots[i].Enabled := false;
              //TVFilter( fImgs[i].Picture.Bitmap );
            end;
      btPrev.Enabled := fOffset > 0;
      btMore.Enabled := fOffset + ActiveSlots < fCriminals.Count;
    end;

  procedure TCriminalRosterView.btMoreClick(Sender: TObject);
    begin
      inc( fOffset );
      fSlots[fActSlot].Color := SocketColor;
      if fActSlot < ActiveSlots - 1
        then inc( fActSlot )
        else fActSlot := -1;
      RenderSlots;
    end;

  procedure TCriminalRosterView.btPrevClick(Sender: TObject);
    begin
      dec( fOffset );
      fSlots[fActSlot].Color := SocketColor;
      if fActSlot > 0
        then dec( fActSlot )
        else fActSlot := -1;
      RenderSlots;
    end;

  procedure TCriminalRosterView.btHireClick(Sender: TObject);
    var
      idx : integer;
    begin
      if fActSlot <> -1
        then
          begin
            idx := fOffset + fActSlot;
            Fork( threadedHireCriminal, priNormal, [TStringList(fCriminals.Objects[idx]).Values['Name'], fLeader.Values['Name'], fTeam.Values['Name']] );
          end;
    end;

  procedure TCriminalRosterView.CloseBtnClick(Sender: TObject);
    begin
      Close;
    end;

end.




