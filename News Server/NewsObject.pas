unit NewsObject;

interface

uses
  ComObj, NewsBoard_TLB, Classes;

type
  TNewsObject = class(TAutoObject, INewsObject)
  public
    function Author: WideString; safecall;
    function Body: WideString; safecall;
    function Date: WideString; safecall;
    function AuthorDesc: WideString; safecall;
    function Importance: Integer; safecall;
    function ReplyCount: Integer; safecall;
    function ReplyAuthor(index: Integer): WideString; safecall;
    function ReplyAuthorDesc(index: Integer): WideString; safecall;
    function ReplyDate(index: Integer): WideString; safecall;
    function ReplySubject(index: Integer): WideString; safecall;
    function ReplySummary(index: Integer): WideString; safecall;
    function ReplyPath(index: Integer): WideString; safecall;
    function Open(const path: WideString): Integer; safecall;
    function OpenReply(index: Integer): Integer; safecall;
    function NewMessage(const Parent, Author, AuthorD, Date, Subject, Body: WideString): Integer; safecall;
    function FolderOnly: WordBool; safecall;
    function Subject: WideString; safecall;
    function BodyHTML: WideString; safecall;
    function GlobalMsgCount: Integer; safecall;
    function GlobalMsgPath(index: Integer): WideString; safecall;
    procedure SetRootPath(const RootPath: WideString); safecall;
    procedure SetIndexSize(size: Integer); safecall;
    function ParentPath: WideString; safecall;
    function Summary: WideString; safecall;
  private
    fPath     : widestring;
    fAuthor   : widestring;
    fAuthorD  : widestring;
    fDate     : widestring;
    fSubject  : widestring;
    fSummary  : widestring;
    fParent   : widestring;
    fBody     : widestring;
    fReplies  : TStringList;
    fFolder   : boolean;
    fRoot     : widestring;
    fIndex    : TStringList;
    fIndexMax : integer;
    fLangId   : string;
  private
    procedure LoadIndex;
  protected
    procedure SetLangId(const aLangId: WideString); safecall;
  end;

implementation

  uses
    Windows, ComServ, CompStringsParser, SysUtils, FileCtrl, NewsRegistry, Registry;

  const
    Separator = '}';

  const
    tidFileName_Header = 'head.txt';
    tidFileName_Main  = 'main.txt';

  const
    tidVal_Author  = 'Author';
    tidVal_AuthorD = 'AuthorD';                                                                                         
    tidVal_Date    = 'Date';                                      
    tidVal_Subject = 'Subject';                                                                        
    tidVal_Summary = 'Summary';
    tidVal_Parent  = 'Parent';

  type
    TReplyInfo =                                                                                             
      class
        Author   : widestring;
        AuthorD  : widestring;
        Date     : widestring;
        Subject  : widestring;
        Summary  : widestring;
      end;
                                                                   
  function CanonicalizePath( path : string ) : string;
    begin
      result := path;
      if result[length(result)] <> '\'
        then result := result + '\';
    end; 

  function GlobalizePath( path, aLangId : string ) : string;
    var
      Reg : TRegistry;
    begin
      if aLangId = ''
        then aLangId := '0';
      if (pos( ':', path ) = 0) and (path[1] <> '\')
        then
          try
            Reg := TRegistry.Create;
            try
              Reg.RootKey := HKEY_LOCAL_MACHINE;
              if Reg.OpenKey( tidRegKey_News, false )
                then result := Reg.ReadString( 'Path' ) + path
                else result := path;
            finally
              Reg.Free;
            end;
          except
            result := path;
          end
        else result := path;                                                           
      result := Format( result, [aLangId] );
    end;
    
  function TNewsObject.Author: WideString;
    begin
      result := fAuthor;
    end;

  function TNewsObject.Body: WideString;
    begin
      result := fBody;
    end;

  function TNewsObject.Date: WideString;
    begin
      result := fDate;
    end;

  function TNewsObject.AuthorDesc: WideString;
    begin
      result := fAuthorD;
    end;

  function TNewsObject.Importance: Integer;                            
    begin
      result := 0;
    end;

  function TNewsObject.ReplyAuthor(index: Integer): WideString;
    begin
      result := TReplyInfo(fReplies.Objects[index]).Author;
    end;

  function TNewsObject.ReplyAuthorDesc(index: Integer): WideString;
    begin
      result := TReplyInfo(fReplies.Objects[index]).AuthorD;
    end;
                                                                          
  function TNewsObject.ReplyCount: Integer;
    begin
      result := fReplies.Count;
    end;

  function TNewsObject.ReplyDate(index: Integer): WideString;              
    begin
      result := TReplyInfo(fReplies.Objects[index]).Date;
    end;

  function TNewsObject.ReplySubject(index: Integer): WideString;
    begin
      result := TReplyInfo(fReplies.Objects[index]).Subject;
    end;

  function TNewsObject.ReplySummary(index: Integer): WideString;
    begin
      result := TReplyInfo(fReplies.Objects[index]).Summary;
    end;

  function TNewsObject.ReplyPath(index: Integer): WideString;
    begin
      result := Format( fReplies[index], [fLangId] );
    end;

  function TNewsObject.Open(const path: WideString): Integer;

    procedure InitReplies;
      var
        found     : integer;
        SearchRec : TSearchRec;
        RepFile   : TStringList;
        RepInfo   : TReplyInfo;
      begin
        found := FindFirst( fPath + '*.msg', faDirectory, SearchRec );
        try
          while found = 0 do
            begin
              RepFile := TStringList.Create;
              try
                RepFile.LoadFromFile( fPath + SearchRec.Name + '\' + tidFileName_Header );
                RepInfo := TReplyInfo.Create;
                RepInfo.Author  := RepFile.Values[tidVal_Author];
                RepInfo.AuthorD := RepFile.Values[tidVal_AuthorD];
                RepInfo.Date    := RepFile.Values[tidVal_Date];
                RepInfo.Subject := RepFile.Values[tidVal_Subject];
                RepInfo.Summary := RepFile.Values[tidVal_Summary];
                fReplies.AddObject( fPath + SearchRec.Name, RepInfo );
              finally
                RepFile.Free;
              end;
              found := FindNext( SearchRec );
            end;
        finally                                              
          FindClose( SearchRec );
        end;
      end;

    var
      Header : TStringList;
      Lines  : TStringList;
      i      : integer;
    begin
      try
        fPath := CanonicalizePath( GlobalizePath( path, fLangId ));
        if fReplies <> nil
          then
            begin
              for i := 0 to pred(fReplies.Count) do
                fReplies.Objects[i].Free;
              fReplies.Free;
              fIndex.Free;
            end;
        if FileExists( fPath + tidFileName_Header )
          then
            begin
              Header := TStringList.Create;
              try
                Header.LoadFromFile( fPath + tidFileName_Header );
                fAuthor   := Header.Values[tidVal_Author];
                fAuthorD  := Header.Values[tidVal_AuthorD];
                fDate     := Header.Values[tidVal_Date];
                fSubject  := Header.Values[tidVal_Subject];
                fSummary  := Header.Values[tidVal_Summary];
                fParent   := Header.Values[tidVal_Parent];
              finally
                Header.Free;
              end;
              Lines := TStringList.Create;
              try
                Lines.LoadFromFile( fPath + tidFileName_Main );
                fBody := Lines.Text;
              finally
                Lines.Free;
              end;
              fFolder := false;
            end
          else fFolder := true;
        fReplies := TStringList.Create;
        InitReplies;
        result := 0;
      except
        result := 1;
      end;
    end;

  function TNewsObject.OpenReply(index: Integer): Integer;
    begin
      result := Open( fReplies[index] ); 
    end;

  function TNewsObject.NewMessage(const Parent, Author, AuthorD, Date, Subject, Body: WideString): Integer;
  
   const
      maxSummary = 60;

    function OrderStr : string;

      function pad( s : string; size : integer ) : string;
        begin
          result := s;
          while length(result) < size do
            result := '0' + result;
        end;

      var
        h, m, s, msec : word;
        y, mn, d : word;
      begin
        DecodeTime( Now, h, m, s, msec );
        DecodeDate( Now, y, mn, d );
        y    := 10000 - y;
        mn   := 12 - mn;
        d    := 32 - d;
        h    := 24 - h;
        m    := 60 - m;
        s    := 60 - s;
        msec := 1000 - msec;
        result := pad(IntToStr(y), 2) + pad(IntToStr(mn), 2) + pad(IntToStr(d), 2) + pad(IntToStr(h), 2) + pad(IntToStr(m), 2) + pad(IntToStr(s), 2) + pad(IntToStr(msec), 4);
      end;

    function AssemblePath : string;
      begin
        result := Parent + OrderStr + '.msg\';
      end;

    function GetSummary( body : string ) : string;
      const
        AllowedChars = ['A'..'Z','a'..'z','0'..'9','(',')','.',',',';','-'];
      var
        i : integer;
        terminate : boolean;
      begin
        terminate := false;
        i := 1;
        result := '';
        while (i <= length(body)) and (length(result) <= maxSummary + 10) and not terminate do
          begin
            if body[i] in AllowedChars
              then result := result + body[i]
              else
                if (result <> '') and (result[length(result)] <> ' ')
                  then result := result + ' ';
            if not terminate and (length(result) > 0) and (result[length(result)] = ' ') and (length(result) > maxSummary - 10)
              then terminate := true;
            inc( i );
          end;
        if result[length(result)] = ' '
          then delete( result, length(result), 1 );
      end;

    procedure UpdateIndex;
      var
        rootPath  : string;
        localPath : string;
        indexFile : TStringList;
      begin
        rootPath  := fRoot;
        indexFile := TStringList.Create;
        try
          localPath := fPath;
          delete( localPath, 1, length(rootPath) );
          indexFile.Add( localPath );
          rootPath := rootPath + 'index\';
          ForceDirectories( rootPath );
          indexFile.SaveToFile( rootPath + OrderStr + '.idx' );
        finally
          indexFile.Free;
        end;
      end;

    var
      Header : TStringList;
      Msg    : TStringList;
    begin
      try
        fAuthor  := Author;
        fAuthorD := AuthorD;
        fDate    := Date;
        fSubject := Subject;
        fSummary := GetSummary( Body );
        fParent  := CanonicalizePath( Parent );
        fBody    := Body;
        fPath    := CanonicalizePath( GlobalizePath( AssemblePath, fLangId ));
        ForceDirectories( fPath );
        // write headers
        Header := TStringList.Create;
        try
          Header.Values[tidVal_Author]  := fAuthor;
          Header.Values[tidVal_AuthorD] := fAuthorD;
          Header.Values[tidVal_Date]    := fDate;
          Header.Values[tidVal_Subject] := fSubject;
          Header.Values[tidVal_Summary] := fSummary;
          Header.Values[tidVal_Parent]  := fParent;
          Header.SaveToFile( fPath + tidFileName_Header );
        finally
          Header.Free;
        end;
        // write body
        Msg := TStringList.Create;
        try
          Msg.Add( Body );
          Msg.SaveToFile( fPath + tidFileName_Main );
        finally
          Msg.Free;
        end;
        result := 0;
      except
        result := 1;
      end;
      UpdateIndex;
    end;

  function TNewsObject.FolderOnly: WordBool;
    begin
      result := fFolder;
    end;

  function TNewsObject.Subject: WideString;
    begin
      result := fSubject;
    end;

  function TNewsObject.BodyHTML: WideString;
    var
      Lines : TStringList;
      line  : string;
      i, j  : integer;
    begin
      result := '';
      Lines := TStringList.Create;
      try
        Lines.Text := fBody;
        for i := 0 to pred(Lines.Count) do                 
          begin
            line := Lines[i];
            j    := 1;
            while (j <= length(line)) and (line[j] = ' ') do
              begin
                delete( line, j, 1 );
                insert( '&nbsp;', line, j );
                inc( j, length('&nbsp;') );
              end;
            result := result + line + '<br>';
          end;
      finally
        Lines.Free;
      end;
    end;

  function TNewsObject.GlobalMsgCount: Integer;
    begin
      if fIndex = nil
        then LoadIndex;
      result := fIndex.Count;
    end;

  function TNewsObject.GlobalMsgPath(index: Integer): WideString;
    begin
      if fIndex = nil
        then LoadIndex;
      if index < fIndex.Count
        then result := fIndex[index]
        else result := ''
    end;

  procedure TNewsObject.LoadIndex;
    var
      SearchRec : TSearchRec;
      found     : integer;
      count     : integer;
      idxPath   : string;
      IdxFile   : TStringList;              
    begin
      if fIndex <> nil
        then fIndex.Free;
      fIndex := TStringList.Create;
      idxPath := fRoot + 'index\';
      count := 0;
      found := FindFirst( idxPath + '*.idx', faArchive, SearchRec );
      try
        while (found = 0) and ((count < fIndexMax) or (fIndexMax = -1)) do
          begin
            IdxFile := TStringList.Create;
            try
              IdxFile.LoadFromFile( idxPath + SearchRec.Name );
              fIndex.Add( fRoot + IdxFile[0] );
            finally
              IdxFile.Free;
            end;
            found := FindNext( SearchRec );
            inc( count );
          end;
      finally
        FindClose( SearchRec );
      end;
    end;

  procedure TNewsObject.SetRootPath(const RootPath: WideString);
    begin
      fRoot := CanonicalizePath( GlobalizePath( RootPath, fLangId ));
    end;

  procedure TNewsObject.SetIndexSize(size: Integer);
    begin
      if size <> fIndexMax
        then
          begin
            fIndex.Free;
            fIndex := nil;
            fIndexMax := size;
          end;
    end;

  function TNewsObject.ParentPath: WideString;
    begin
      result := CanonicalizePath( GlobalizePath( fParent, fLangId ));
    end; 

  function TNewsObject.Summary: WideString;
    begin
      result := fSummary;
    end;

  procedure TNewsObject.SetLangId(const aLangId: WideString);
    begin
      fLangId := aLangId;
    end;

initialization

  TAutoObjectFactory.Create(ComServer, TNewsObject, Class_NewsObject, ciMultiInstance);

end.

