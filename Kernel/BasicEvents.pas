unit BasicEvents;

interface

  uses
    Events, Classes, BackupInterfaces, Languages;

  const
    evnKind_FacEvent = 1;

  type
    TFacEvent =
      class( TEvent )
        public
          constructor Create(
            aDateTick : integer;
            aDate : TDateTime;
            aTTL, aPriority : integer;
            aMetaFacility : string;
            aText : TMultiString;
            Tycoon, Town : string;
            aSender, aURL : string );
        private
          fMetaFacility : string;
          fCount        : integer;
          fTycoon       : string;
          fTown         : string;
        public
          property MetaFacility : string  read fMetaFacility;
          property Count        : integer read fCount;
        public
          function  CanAssimilate( Event : TEvent ) : boolean; override;
          procedure Assimilate( Event : TEvent ); override;
          function  Render : string; override;
          function  Clone : TEvent; override;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

  procedure RegisterBackup;

implementation

  uses
    Protocol, Kernel, MetaInstances, ClassStorage, SysUtils;

  constructor TFacEvent.Create( aDateTick       : integer;
                                aDate           : TDateTime;
                                aTTL, aPriority : integer;
                                aMetaFacility   : string;
                                aText           : TMultiString;
                                Tycoon, Town    : string;
                                aSender, aURL   : string );
    begin
      inherited Create( evnKind_FacEvent, aDateTick, aDate, aTTL, aPriority, aText, aSender, aURL );
      fMetaFacility := aMetaFacility;
      fTycoon       := Tycoon;
      fTown         := Town;
      fCount        := 1;
    end;

  function TFacEvent.CanAssimilate( Event : TEvent ) : boolean;
    begin
      result := ObjectIs( TFacEvent.ClassName, Event ) and (MetaFacility = TFacEvent(Event).MetaFacility)
    end;

  procedure TFacEvent.Assimilate( Event : TEvent );
    begin
      inc( fCount, TFacEvent(Event).Count );
    end;

  function TFacEvent.Render : string;
    var
      list : TStringList;
      enum : string;
      MF   : TMetaFacility;
      i    : integer;
    begin
      list := TStringList.Create;
      try
        list.Text := inherited Render;
        MF := TMetaFacility(TheClassStorage.ClassById[tidClassFamily_Facilities, MetaFacility]);
        for i := 0 to pred(LangList.Count) do
          begin
            enum := IntToStr(Count);
            if Count > 1
              then enum := enum + ' ' + MF.PluralName_MLS.Values[LangList[i]]
              else enum := enum + ' ' + MF.Name_MLS.Values[LangList[i]];
            list.Values[tidEventField_Text + IntToStr(i)] := Format( Text.Values[IntToStr(i)], [fTycoon, enum, fTown] );
          end;
        result := list.Text;
      finally                                 
        list.Free;
      end;
    end;

  function TFacEvent.Clone : TEvent;
    begin
      result := TFacEvent.Create( DateTick, Date, TTL, Priority, MetaFacility, CloneMultiString( Text ), fTycoon, fTown, Sender, URL );
      TFacEvent(result).fCount := fCount;
    end;

  procedure TFacEvent.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      fMetaFacility := Reader.ReadString( 'MetaFacility', '' );
      fCount        := Reader.ReadInteger( 'Count', 0 );
    end;

  procedure TFacEvent.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteString( 'MetaFacility', fMetaFacility );
      Writer.WriteInteger( 'Count', fCount );
    end;


  // RegisterBackup

  procedure RegisterBackup;
    begin
      RegisterClass( TFacEvent );
    end;

end.


