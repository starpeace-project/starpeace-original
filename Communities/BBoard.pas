unit BBoard;

interface

uses
  {$IFNDEF TEST}
  ComObj, ActiveX, commmunity_TLB, StdVcl, Classes, Windows;
  {$ELSE}
  ComObj, ActiveX, Classes, Windows;
  {$ENDIF}

type
  TBBoard = class(TAutoObject, IBBoard)
  public
    destructor Destroy; override;
  public
    procedure Open(const Id: WideString; IsForum: WordBool); safecall;
    function  Count: SYSINT; safecall;
    procedure NewItem(const Author, Title, ParentUId, Body: WideString; Topic: WordBool); safecall;
    procedure NewItemEx(const Author, Title, AttchName, ParentUId, Body: WideString; Topic: WordBool); safecall;
    function  GetAuthor(index: Integer): WideString; safecall;
    function  GetTitle(index: Integer): WideString; safecall;
    function  GetId(index: Integer): WideString; safecall;
    function  GetRepliesCount(index: Integer): SYSINT; safecall;

    procedure NewPoll(const Author, Title, ParentUId, Body, PollItems: WideString); safecall;
    function  PollItemsCount: SYSINT; safecall;
    function  VotesCount: SYSINT; safecall;
    function  GetPollItem(index: Integer): WideString; safecall;
    function  GetVotes(index: Integer): SYSINT; safecall;
    procedure AddVote(const Author, ParentUId, Body: WideString; PollItemIdx: Integer); safecall;
    { Protected declarations }
  private
    fId         : string;
    fItems      : TStringList;
    fIndex      : TStringList;
    fCount      : integer;
    fIsForum    : boolean;
    fPollItems  : TStringList;
    fVotesCount : integer;
    fVotes      : TStringList;
    fUploadDir  : string;
  public
    function GetBody(index: Integer): WideString; safecall;
    function GetDate(index: Integer): TDateTime; safecall;
    function GetHTMLBody(index: Integer): WideString; safecall;
    function GetAttachment(index : integer): WideString; safecall;
  private
    function GetPCount(folder: string): integer; safecall;
  protected
    function Get_ThreadFolder: WideString; safecall;
    function Get_UploadDir: WideString; safecall;
  end;

implementation

uses
  ComServ, SysUtils, CompStringsParser, BaseUtils, FileCtrl, StrUtils;

const
  Separator = '}';
  CodeChar  = '{';

const
  CodeEqv : array[0..9] of char = ('\', '/', ':', '"', '?', '*', '|', '<', '>', CodeChar);

function EncodeStr( str : string ) : string;
  var
    i : integer;
    c : string;
  begin
    result := '';
    for i := 1 to length(str) do
      begin
        c := str[i];
        case c[1] of
          '\' : c := CodeChar + '00';
          '/' : c := CodeChar + '01';
          ':' : c := CodeChar + '02';
          '"' : c := CodeChar + '03';
          '?' : c := CodeChar + '04';
          '*' : c := CodeChar + '05';
          '|' : c := CodeChar + '06';
          '<' : c := CodeChar + '07';
          '>' : c := CodeChar + '08';
          CodeChar : c := CodeChar + '09';
        end;
        result := result + c;
      end
  end;

function DecodeStr( str : string ) : string;
  var
    p1, p2 : integer;
    s      : string;
    c      : string;
    x      : integer;
  begin
    result := '';
    p1 := 1;
    repeat
      p2 := p1;
      p1 := pos( CodeChar, str, p2 );
      if p1 = 0
        then p1 := length(str) + 1;
      s  := copy( str, p2, p1 - p2 );
      result := result + s;
      if p1 < length(str)
        then
          begin
            c := copy( str, p1 + 1, 2 );
            x := StrToInt( c );
            result := result + CodeEqv[x];
            inc( p1, 3 );
          end;
    until p1 >= length(str);
  end;

const
  tidPrefix_Author           = 'Author';
  tidPrefix_Subject          = 'Subject';
  tidPrefix_Id               = 'Id';
  tidPrefix_UId              = 'UId';
  tidPrefix_TimeStamp        = 'TimeStamp';
  tidPrefix_Kind             = 'Kind';
  tidPrefix_AttchName        = 'AttchName';
  tidFileExt_Topic           = '.topic';
  tidFileExt_Post            = '.post';
  tidProperty_Modified       = 'Modified';
  tidProperty_PostCount      = 'PostCount';
  tidProperty_VotesCount     = 'VotesCount';
  tidProperty_PollItemsCount = 'PollItemCount';
  tidProperty_PollItemDesc   = 'PollItemDesc';
  tidProperty_PollItemVotes  = 'PollItemVotes';
  tidFileName_TopicIdx       = 'topic.index';

type
  TItemData =
    record
      Author    : string;
      Subject   : string;
      Id        : string;
      UId       : string;
      TimeStamp : string;
      Kind      : integer;
      AttchName : string;
    end;

function GetItemData( path : string ) : TItemData;
  var
    Name : string;
    kind : string;
    p    : integer;
  begin
    Name             := ExtractFileName( path );
    p                := 1;
    result.Id        := path;
    result.Author    := GetNextStringUpTo( Name, p, Separator );
    result.Subject   := GetNextStringUpTo( Name, p, Separator );
    result.UId       := GetNextStringUpTo( Name, p, Separator );
    kind             := GetNextStringUpTo( Name, p, Separator );
    if kind <> ''
      then result.Kind := StrToInt(kind)
      else result.Kind := 0;
    if result.Kind <> 0
      then result.AttchName := GetNextStringUpTo(Name, p, Separator)
      else result.AttchName := '';
  end;

procedure RenderItemDataToList( const Data : TItemData; index : integer; List : TSTringList );
  var
    postfix : string;
  begin
    postfix := IntToStr( index );
    List.Values[tidPrefix_Author  + postfix] := Data.Author;
    List.Values[tidPrefix_Subject + postfix] := Data.Subject;
    List.Values[tidPrefix_Id      + postfix] := Data.Id;
    List.Values[tidPrefix_UId     + postfix] := Data.UId;
    List.Values[tidPrefix_Kind    + postfix] := IntToStr(Data.Kind);
    List.Values[tidPrefix_AttchName + postfix] := Data.AttchName;
  end;

function CreateItemName( Author, Subject, UId : string ) : string;
  begin
    result :=
      Author + Separator +
      Subject + Separator +
      UId;            
  end;

function CreateItemNameEx( Author, Subject, UId, AttchName : string ) : string;
  begin
    result :=
      Author + Separator +
      Subject + Separator +
      UId + Separator +
      '99' + Separator +
      AttchName + Separator;
  end;

destructor TBBoard.Destroy;
  begin
    fItems.Free;
    fIndex.Free;
    inherited;
  end;

procedure TBBoard.Open(const Id: WideString; IsForum: WordBool);
  var
    SearchRec : TSearchRec;
    found     : integer;
    Data      : TItemData;
    path      : string;
    systime   : TSystemTime;
    pattern   : string;
    TopicIdx  : TStringList;
    DateStamp : string;
    itemCount : integer;
    i         : integer;
  begin
    try
      if fItems <> nil
        then fItems.Free;
      if fIndex <> nil
        then fIndex.Free;
      if fPollItems <> nil
        then fPollItems.Free;
      if fVotes <> nil
        then fVotes.Free;
      fItems := TStringList.Create;
      fIndex := TStringList.Create;
      fIndex.Sorted := true;
      fIndex.Duplicates := dupIgnore;
      fIsForum := IsForum;
      fPollItems := TStringList.Create;
      fVotes     := TStringList.Create;
      fId  := Id;
      path := fId + '\';
      fCount := 0;
      if fIsForum
        then pattern := tidFileExt_Topic
        else
          begin
            TopicIdx := TStringList.Create;
            try
              TopicIdx.LoadFromFile( path + tidFileName_TopicIdx );
              try
                fVotesCount := StrToInt( TopicIdx.Values[tidProperty_VotesCount] );
              except
                fVotesCount := 0;
              end;
              if TopicIdx.Values[tidProperty_PollItemsCount] <> '' then
                begin
                  itemCount := StrToInt( TopicIdx.Values[tidProperty_PollItemsCount] );
                  for i := 0 to itemCount - 1 do
                    begin
                      fPollItems.Add( TopicIdx.Values[tidProperty_PollItemDesc + IntToStr(i+1)] );
                      fVotes.Add( TopicIdx.Values[tidProperty_PollItemVotes + IntToStr(i+1)] );
                    end;
                end;
            finally
              TopicIdx.Free;
            end;
            pattern := tidFileExt_Post;
          end;
      found := FindFirst( path + '*' + pattern, faAnyFile, SearchRec );
      try
        while found = 0 do
          begin
            Data := GetItemData( path + SearchRec.Name );
            RenderItemDataToList( Data, fCount, fItems );
            if fIsForum
              then
                begin
                  TopicIdx := TStringList.Create;
                  try
                    TopicIdx.LoadFromFile( path + SearchRec.Name + '\' + tidFileName_TopicIdx );
                    DateStamp := TopicIdx.Values[tidProperty_Modified];
                  finally
                    TopicIdx.Free;
                  end;
                end
              else
                begin
                  FileTimeToSystemTime( SearchRec.FindData.ftLastWriteTime, systime );
                  SystemTimeToTzSpecificLocalTime( nil, systime, systime );
                  DateStamp := FloatToStr(SystemTimeToDateTime( systime ));
                end;
            fIndex.Add( DateStamp + '=' + IntToStr(fCount) );
            fItems.Values[tidPrefix_TimeStamp + IntToStr(fCount)] := DateStamp;
            inc( fCount );
            found := FindNext( SearchRec );
          end;
      finally
        FindClose( SearchRec );
      end;
    except
    end;
  end;

function TBBoard.Count: SYSINT;
  begin
    try
      result := fIndex.Count;
    except
      result := 0;
    end;
  end;

procedure TBBoard.NewItem(const Author, Title, ParentUId, Body: WideString; Topic: WordBool);
  var
    name     : string;
    MsgFile  : TStringList;
    Folder   : string;
    RepCount : integer;
  begin
    try
      name := CreateItemName( Author, EncodeStr( Title ), Author + DateTimeToAbc( Now ) );
      folder := ParentUId;
      if folder <> '\'
        then folder := folder + '\';
      if Topic
        then
          begin
            folder := folder + name + tidFileExt_Topic + '\';
            ForceDirectories( folder );
          end;
      fUploadDir := folder;
      RepCount := GetPCount( folder );
      MsgFile  := TStringList.Create;
      try
        if FileExists( folder + tidFileName_TopicIdx )
          then MsgFile.LoadFromFile( folder + tidFileName_TopicIdx );
        MsgFile.Values[tidProperty_Modified] := FloatToStr( Now );
        MsgFile.Values[tidProperty_PostCount] := IntToStr( RepCount + 1 );
        MsgFile.SaveToFile( folder + tidFileName_TopicIdx );
      finally
        MsgFile.Free;
      end;
      MsgFile := TStringList.Create;
      try
        MsgFile.Text := Body;
        MsgFile.SaveToFile( folder + name + tidFileExt_Post );
      finally
        MsgFile.Free;
      end;
    except
    end;
  end;

procedure TBBoard.NewItemEx(const Author, Title, AttchName, ParentUId, Body: WideString; Topic: WordBool);
  var
    name     : string;
    MsgFile  : TStringList;
    Folder   : string;
    RepCount : integer;
  begin
    try
      name := CreateItemNameEx(Author, EncodeStr(Title), Author + DateTimeToAbc(Now), AttchName);
      folder := ParentUId;
      if folder <> '\'
        then folder := folder + '\';
      if Topic
        then
          begin
            folder := folder + name + tidFileExt_Topic + '\';
            ForceDirectories( folder );
          end;
      fUploadDir := folder;
      RepCount := GetPCount( folder );
      MsgFile  := TStringList.Create;
      try
        if FileExists( folder + tidFileName_TopicIdx )
          then MsgFile.LoadFromFile( folder + tidFileName_TopicIdx );
        MsgFile.Values[tidProperty_Modified] := FloatToStr( Now );
        MsgFile.Values[tidProperty_PostCount] := IntToStr( RepCount + 1 );
        MsgFile.SaveToFile( folder + tidFileName_TopicIdx );
      finally
        MsgFile.Free;
      end;
      MsgFile := TStringList.Create;
      try
        MsgFile.Text := Body;
        MsgFile.SaveToFile( folder + name + tidFileExt_Post );
      finally
        MsgFile.Free;
      end;
    except
    end;
  end;

function GetIndex( indexstr : string ) : string;
  begin
    result := copy( indexstr, system.pos( '=', indexstr ) + 1, length(indexstr) );
  end;

function TBBoard.GetAuthor(index: Integer): WideString;
  begin
    try
      if fIsForum
        then index := fIndex.Count - 1 - index;
      result := fItems.Values[tidPrefix_Author + GetIndex(fIndex[index])];
    except
      result := '';
    end;
  end;

function TBBoard.GetTitle(index: Integer): WideString;
  begin
    try
      if fIsForum
        then index := fIndex.Count - 1 - index;
      result := DecodeStr( fItems.Values[tidPrefix_Subject + GetIndex(fIndex[index])] );
    except
      result := '';
    end;
  end;

function TBBoard.GetId(index: Integer): WideString;
  begin
    try
      if fIsForum
        then index := fIndex.Count - 1 - index;
      result := fItems.Values[tidPrefix_Id + GetIndex(fIndex[index])];
    except
      result := '';
    end;
  end;

function TBBoard.GetRepliesCount(index: Integer): SYSINT;
  var
    folder : string;
  begin
    try
      folder := GetId( index ) + '\';
      result := GetPCount( folder );
    except
      result := -1;
    end;
  end;

function TBBoard.GetAttachment(index: Integer): WideString;
  begin
    try
      if fIsForum
        then index := fIndex.Count - 1 - index;
      result := DecodeStr( fItems.Values[tidPrefix_AttchName + GetIndex(fIndex[index])] );
    except
      result := '';
    end;
  end;

procedure TBBoard.NewPoll(const Author, Title, ParentUId, Body, PollItems: WideString);
  var
    name    : string;
    MsgFile : TStringList;
    Items   : TStringList;
    Folder  : string;
    i       : integer;
  begin
    try
      name := CreateItemName( Author, EncodeStr( Title ), Author + DateTimeToAbc( Now ) );
      folder := ParentUId;
      if folder <> '\'
        then folder := folder + '\';
      folder := folder + name + tidFileExt_Topic + '\';
      ForceDirectories( folder );
      MsgFile := TStringList.Create;
      try
        MsgFile.Values[tidProperty_Modified] := FloatToStr( Now );
        MsgFile.Values[tidProperty_PostCount] := '1';
        Items := TStringList.Create;
        try
          Items.Text := PollItems;
          MsgFile.Values[tidProperty_VotesCount]     := '0';
          MsgFile.Values[tidProperty_PollItemsCount] := IntToStr( Items.Count );
          if Items.Count > 0 then
            for i := 0 to Items.Count - 1 do
              begin
                MsgFile.Values[tidProperty_PollItemDesc + IntToStr(i+1)]  := Items[i];
                MsgFile.Values[tidProperty_PollItemVotes + IntToStr(i+1)] := '0';
              end;
        finally
          Items.Free;
        end;
        MsgFile.SaveToFile( folder + tidFileName_TopicIdx );
      finally
        MsgFile.Free;
      end;
      MsgFile := TStringList.Create;
      try
        MsgFile.Text := Body;
        MsgFile.SaveToFile( folder + name + tidFileExt_Post );
      finally
        MsgFile.Free;
      end;
    except
    end;
  end;

function TBBoard.PollItemsCount: SYSINT;
  begin
    try
      result := fPollItems.Count;
    except
      result := -1;
    end;
  end;

function TBBoard.VotesCount: SYSINT;
  begin
    result := fVotesCount;
  end;

function TBBoard.GetPollItem(index: Integer): WideString;
  begin
    try
      result := fPollItems[index];
    except
      result := '';
    end;
  end;

function TBBoard.GetVotes(index: Integer): SYSINT;
  begin
    try
      result := StrToInt( fVotes[index] );
    except
      result := -1;
    end;
  end;

procedure TBBoard.AddVote(const Author, ParentUId, Body: WideString; PollItemIdx: Integer);
  var
    name     : string;
    MsgFile  : TStringList;
    Folder   : string;
    HaveComm : boolean;
  begin
    try
      name   := CreateItemName( Author, '', Author + DateTimeToAbc( Now ) );
      folder := ParentUId;
      if folder <> '\'
        then folder := folder + '\';
      HaveComm := Trim( Body ) <> '';
      MsgFile  := TStringList.Create;
      try
        MsgFile.LoadFromFile( folder + tidFileName_TopicIdx );
        MsgFile.Values[tidProperty_Modified]   := FloatToStr( Now );
        MsgFile.Values[tidProperty_VotesCount] := IntToStr( StrToInt( MsgFile.Values[tidProperty_VotesCount] ) + 1 );
        if HaveComm
          then MsgFile.Values[tidProperty_PostCount]  := IntToStr( StrToInt( MsgFile.Values[tidProperty_PostCount] ) + 1 );
        if (PollItemIdx > 0) and (PollItemIdx <= StrToInt( MsgFile.Values[tidProperty_PollItemsCount] )) then
          MsgFile.Values[tidProperty_PollItemVotes + IntToStr(PollItemIdx)] := IntToStr( StrToInt( MsgFile.Values[tidProperty_PollItemVotes + IntToStr(PollItemIdx)] ) + 1 );
        MsgFile.SaveToFile( folder + tidFileName_TopicIdx );
      finally
        MsgFile.Free;
      end;
      if HaveComm then
        begin
          MsgFile := TStringList.Create;
          try
            MsgFile.Text := Body;
            MsgFile.SaveToFile( folder + name + tidFileExt_Post );
          finally
            MsgFile.Free;
          end;
        end;
    except
    end;
  end;

function TBBoard.GetBody(index: Integer): WideString;
  var
    path : string;
    body : TStringList;
  begin
    try
      if not fIsForum
        then
          begin
            path := fItems.Values[tidPrefix_Id + GetIndex(fIndex[index])];
            if path <> ''
              then
                begin
                  body := TStringList.Create;
                  try
                    body.LoadFromFile( path );
                    result := body.Text;
                  finally
                    body.Free;
                  end;
                end
              else result := '';
          end
        else result := '';
    except
      result := '';
    end;
  end;

function TBBoard.GetDate(index: Integer): TDateTime;
  var
    datestamp : string;
    numdate   : double;
  begin
    try
      if fIsForum
        then index := fIndex.Count - 1 - index;
      datestamp := fItems.Values[tidPrefix_TimeStamp + GetIndex(fIndex[index])];
      if datestamp <> ''
        then
          begin
            numdate := StrToFloat( datestamp );
            result  := TDateTime(numdate);
          end
        else result := Now;
    except
      result := Now;
    end;
  end;

function TBBoard.GetHTMLBody(index: Integer): WideString;
  var
    rawbody : TStringList;
    i       : integer;
  begin
    try
      rawbody := TStringList.Create;
      try
        rawbody.Text := GetBody( index );
        result := '';
        for i := 0 to pred(rawbody.Count) do
          result := result + rawbody[i] + '<br>';
      finally
        rawbody.Free;
      end;
    except
      result := '';
    end;
  end;

function TBBoard.GetPCount(folder: string): integer; safecall;
  var
    MsgFile : TStringList;
  begin
    result := 0;
    if FileExists( folder + tidFileName_TopicIdx )
      then
        begin
          MsgFile := TStringList.Create;
          try
            MsgFile.LoadFromFile( folder + tidFileName_TopicIdx );
            if MsgFile.Values[tidProperty_PostCount] <> ''
              then
                try
                  result := StrToInt( MsgFile.Values[tidProperty_PostCount] );
                except
                end;
          finally
            MsgFile.Free;
          end;
        end;
  end;

function TBBoard.Get_ThreadFolder : WideString;
  begin
    result := ExtractFileName(fId);
  end;

function TBBoard.Get_UploadDir: WideString;
  begin
    result := fUploadDir;
  end;

initialization

  {$IFNDEF TEST}
  TAutoObjectFactory.Create(ComServer, TBBoard, Class_BBoard, ciMultiInstance, tmApartment);
  {$ENDIF}

end.

