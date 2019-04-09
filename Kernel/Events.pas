unit Events;

interface

  uses
    Classes, Persistent, BackupInterfaces, Languages, Variants;

  type
    TEventKind = integer;

  type
    TEvent =
      class( TPersistent )
        private
          fKind     : TEventKind;
          fDateTick : integer;
          fDate     : TDateTime;
          fTTL      : integer;
          fPriority : integer;
          fText     : TMultiString;
          fSender   : string;
          fURL      : string;
        public
          property Kind     : TEventKind read fKind;
          property DateTick : integer    read fDateTick;
          property Date     : TDateTime  read fDate;
          property TTL      : integer    read fTTL;
          property Priority : integer    read fPriority;
          property Text     : TMultiString     read fText;
          property Sender   : string     read fSender;
          property URL      : string     read fURL;
        public
          constructor Create( aKind : TEventKind;
                              aDateTick : integer;
                              aDate : TDateTime;
                              aTTL, aPriority : integer;
                              aText : TMultiString;
                              aSender, aURL : string );
          destructor Destroy; override;
        public
          function  CanAssimilate( Event : TEvent ) : boolean; virtual;
          procedure Assimilate( Event : TEvent ); virtual;
          function  GetPrecedence : integer; virtual;
          function  Render : string; virtual;
          function  Clone : TEvent; virtual;
        protected
          procedure LoadFromBackup( Reader : IBackupReader ); override;
          procedure StoreToBackup ( Writer : IBackupWriter ); override;
      end;

  procedure RegisterBackup;

implementation

  uses
    SysUtils, Protocol, Logs;

  // TEvent

  constructor TEvent.Create( aKind : TEventKind; aDateTick : integer; aDate : TDateTime; aTTL, aPriority : integer; aText : TMultiString; aSender, aURL : string );
    begin
      inherited Create;
      fKind     := aKind;
      fDateTick := aDateTick;
      fDate     := aDate;
      fTTL      := aTTL;
      fPriority := aPriority;
      fText     := aText;
      fSender   := aSender;
      fURL      := aURL;
    end;

  destructor TEvent.Destroy; 
    begin
      {
      try
        Logs.Log( 'Demolition', TimeToStr(Now) + ' - ' + ClassName );
      except
      end;
      }
      fText.Free;
      inherited;
    end;

  function TEvent.CanAssimilate( Event : TEvent ) : boolean;
    begin
      result := false;
    end;

  procedure TEvent.Assimilate( Event : TEvent );
    begin
    end;

  function TEvent.GetPrecedence : integer;
    begin
      result := Priority - TTL;
    end;

  function TEvent.Render : string;
    var
      list : TStringList;
      i    : integer;
    begin
      list := TStringList.Create;
      try
        list.Values[tidEventField_Date] := DateToStr(Date);
        list.Values[tidEventField_Kind] := IntToStr(Kind);
        list.Values[tidEventField_URL]  := URL;
        for i := 0 to pred(Text.Count) do
          list.Values[tidEventField_Text + IntToStr(i)] := Text.Values[IntToStr(i)];
        result := list.Text;
      finally
        list.Free;
      end;
    end;

  function TEvent.Clone : TEvent;
    begin
      result := TEvent.Create( Kind, DateTick, Date, TTL, Priority, CloneMultiString( Text ), Sender, URL );
    end;
    
  procedure TEvent.LoadFromBackup( Reader : IBackupReader );
    begin
      inherited;
      fKind     := Reader.ReadInteger( 'Kind', 0 );
      fDateTick := Reader.ReadInteger( 'DateTick', 0 );
      fDate     := StrToDate(Reader.ReadString( 'Date', '0/0/0' ));
      fTTL      := Reader.ReadInteger( 'TTL', 0 );
      fPriority := Reader.ReadInteger( 'Priority', 0 );
      Reader.ReadObject( 'Text_MLS', fText, nil );
      if fText = nil
        then fText := TMultiString.Create;
      fSender := Reader.ReadString( 'Sender', '' );
    end;

  procedure TEvent.StoreToBackup( Writer : IBackupWriter );
    begin
      inherited;
      Writer.WriteInteger( 'Kind', fKind );
      Writer.WriteInteger( 'DateTick', fDateTick );
      Writer.WriteString( 'Date', DateToStr(fDate) );
      Writer.WriteInteger( 'TTL', fTTL );
      Writer.WriteInteger( 'Priority', fPriority );
      Writer.WriteLooseObject( 'Text_MLS', fText );
      Writer.WriteString( 'Sender', fSender );
    end;

    
  // RegisterBackup;

  procedure RegisterBackup;
    begin
      RegisterClass( TEvent );
    end;
    

end.


