unit BasicCurriculum;

interface

  uses
    Kernel, BackupInterfaces, Languages, Variants;

  type
    TOpenItem =
      class( TCurriculumItem )
        public
          constructor Create( anId : string; aKind : integer; aDesc : TMultiString; anImportance : integer; aPrestige : TPrestige );
        private
          fDesc_MLS   : TMultiString;
          fImportance : integer;
          fPrestige   : TPrestige;
        protected
          function GetDesc( langId : TLanguageId ) : string;    override;
          function GetImportance : integer;   override;
          function GetPrestige   : TPrestige; override;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

    TOwnershipItem =
      class( TCurriculumItem )
        protected
          function GetDesc( langId : TLanguageId ) : string;    override;
          function GetImportance : integer;   override;
          function GetPrestige   : TPrestige; override;
        public
          class function IsGlobal : boolean; override;
      end;

    TResearchItem =
      class( TCurriculumItem )
        protected
          function GetDesc( langId : TLanguageId ) : string;    override;
          function GetImportance : integer;   override;
          function GetPrestige   : TPrestige; override;
        public
          class function IsGlobal : boolean; override;
      end;

    TCustomItem =
      class( TCurriculumItem )
        public
          constructor Create(anId : string; aKind : integer; aPrestige : TPrestige; aMsg : string);
        private
          fMessage  : string;
          fPrestige : TPrestige;
        protected
          function  GetDesc(langId : TLanguageId) : string;    override;
          function  GetImportance : integer;   override;
          function  GetPrestige   : TPrestige; override;
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;


  const
    currKind_Research = 2;

  procedure RegisterBackup;

implementation

  uses
    SysUtils, SimHints, MathUtils;


  // TOpenItem

  constructor TOpenItem.Create( anId : string; aKind : integer; aDesc : TMultiString; anImportance : integer; aPrestige : TPrestige );
    begin
      inherited Create( anId, aKind );
      fDesc_MLS   := aDesc;
      fImportance := anImportance;
      fPrestige   := aPrestige;
    end;

  function TOpenItem.GetDesc( langId : TLanguageId ) : string;
    begin
      result := fDesc_MLS.Values[langId];
    end;

  function TOpenItem.GetImportance : integer;
    begin
      result := fImportance;
    end;

  function TOpenItem.GetPrestige : TPrestige;
    begin
      result := fPrestige;
    end;

  procedure TOpenItem.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      Reader.ReadObject( 'Desc_MLS', fDesc_MLS, nil );
      if fDesc_MLS = nil
        then fDesc_MLS := TMultiString.Create;
      fImportance := Reader.ReadInteger( 'Importance', 0 );
      fPrestige   := Reader.ReadSingle( 'Prestige', 0 );
    end;

  procedure TOpenItem.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteLooseObject( 'Desc_MLS', fDesc_MLS );
      Writer.WriteInteger( 'Importance', fImportance );
      Writer.WriteSingle( 'Prestige', fPrestige );
    end;


  // TOwnershipItem

  function TOwnershipItem.GetDesc( langId : TLanguageId ) : string;
    var
      FacCount : integer;
      i        : integer;
    begin
      FacCount := 0;
      try
        Tycoon.Companies.Lock;
        try
          for i := 0 to pred(Tycoon.Companies.Count) do
            inc( FacCount, TCompany(Tycoon.Companies[i]).Facilities.Count );
        finally
          Tycoon.Companies.Unlock;
        end;
        result := SimHints.GetHintText(mtidOwnershipReport.Values[langId], [Tycoon.Companies.Count, FacCount, Tycoon.Area, FormatMoney(Tycoon.AreaTax)]);
      except
        result := '';
      end;
    end;

  function TOwnershipItem.GetImportance : integer;
    begin
      result := 0;
    end;

  function TOwnershipItem.GetPrestige : TPrestige;
    begin
      result := Tycoon.FacPrestige;
    end;

  class function TOwnershipItem.IsGlobal : boolean;
    begin
      result := false;
    end;


  // TResearchItem

  function TResearchItem.GetDesc( langId : TLanguageId ) : string;
    begin
      try
        result := SimHints.GetHintText( mtidResearchReport.Values[langId], [Tycoon.ResearchCount] );
      except
        result := '';
      end;
    end;

  function TResearchItem.GetImportance : integer;
    begin
      result := 0;
    end;

  function TResearchItem.GetPrestige : TPrestige;
    begin
      result := Tycoon.ResearchPrest;
    end;

  class function TResearchItem.IsGlobal : boolean;
    begin
      result := false;
    end;


  // TCustomItem

  constructor TCustomItem.Create(anId : string; aKind : integer; aPrestige : TPrestige; aMsg : string);
    begin
      inherited Create(anId, aKind);
      fPrestige := aPrestige;
      fMessage  := aMsg;
    end;

  function TCustomItem.GetDesc( langId : TLanguageId ) : string;
    begin
      result := Format(fMessage, [round(fPrestige)]);
    end;

  function TCustomItem.GetImportance : integer;
    begin
      result := inherited GetImportance;
    end;

  function TCustomItem.GetPrestige   : TPrestige;
    begin
      result := fPrestige;
    end;

  procedure TCustomItem.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      fPrestige := Reader.ReadSingle('Prestige', 0);
      fMessage  := Reader.ReadString('Msg', '');
    end;

  procedure TCustomItem.StoreToBackup ( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteSingle('Prestige', fPrestige);
      Writer.WriteString('Msg', fMessage);
    end;


  // RegisterBackup

  procedure RegisterBackup;
    begin
      RegisterClass( TOpenItem );
      RegisterClass( TOwnershipItem );
      RegisterClass( TResearchItem );
    end;

end.

