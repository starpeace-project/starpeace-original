unit BlockLevels;

interface

  uses
    Collection, SysUtils, Classes, BackupConsts;

  const
    BeginRange = '[';
    RangeSep   = ',';
    EndRange   = ']';

  type
    TCollection       = Collection.TCollection;
    TLevelManager     = class;
    TMultiLevelStream = class;

    TLevelManager =
      class
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fLevelFiles : TCollection;
          fTargetFile : TStream;
          fLevelPath  : string;
          fLineNumber : integer;
        public
          function  CreateLevels(aStream : TStream; const Path : string) : boolean;
          procedure Reset;
        private
          function CreateFile(const path : string; level : integer) : boolean;
          function ReadLine(level : integer; var line : string) : boolean;
          function WriteLine(level : integer; const line : string) : boolean;
          function GetLevelPos(level : integer) : integer;
          function ReadLevel(level : integer) : boolean;
        private
          function GetLevel(index : integer) : TStream;
          function GetLevelCount : integer;
        public
          property LevelCount : integer read GetLevelCount;
          property Levels[index : integer] : TStream read GetLevel; default;
      end;

    Bounds = 0..100;

    TLevelBound =
      record
        LvStart : integer;
        LvEnd   : integer;
      end;

    PLevelBounds = ^TLevelBounds;
    TLevelBounds = array[Bounds] of TLevelBound;

    TMultiLevelStream =
      class
        public
          constructor Create(aStream : TStream; const Path : string);
          destructor  Destroy; override;
        private
          fLevelMan    : TLevelManager;
          fLevelProps  : TCollection;
          fCurLevel    : integer;
          fLevelBounds : PLevelBounds;
          fLog         : TStream;
        private
          function ReadPropValue(const Name : string; var Prop : string; var eob : boolean) : boolean;
        public
          function  ReadString(const Name, DefVal : string) : string;
          function  ReadLevel (const Name, DefVal : string) : string;
          procedure EnterLevel;
          procedure LeaveLevel;
          procedure LogText(text : string);
        private
          fLargestLevel : TStream;
          fLargestSize  : integer;
        private
          function GetPosition : integer;
          function GetSize : integer;
        public
          property Position   : integer read GetPosition;
          property Size       : integer read GetSize;
          property Log : TStream read fLog write fLog;
        private
          function  GetLevelStart : integer;
          procedure SetLevelStart(offset : integer);
          function  GetLevelEnd : integer;
          procedure SetLevelEnd(offset : integer);
        protected
          property LevelStart : integer read GetLevelStart write SetLevelStart;
          property LevelEnd   : integer read GetLevelEnd   write SetLevelEnd;
      end;

    EBlockLevelError = class(Exception);

implementation

  uses
    CompStringsParser, DelphiStreamUtils;

  function SkipSpaces(str : pchar) : pchar;
    begin
      while (str[0] = ' ') and (str[0] <> #0) do
        inc(str);
      result := str;
    end;

  // TLevelManager

  constructor TLevelManager.Create;
    begin
      inherited;
      fLevelFiles := TCollection.Create(0, rkBelonguer);
      fTargetFile := nil;
      fLineNumber := 1;
    end;

  destructor TLevelManager.Destroy;
    begin
      fLevelFiles.Free;
      inherited;
    end;

  function TLevelManager.CreateLevels(aStream : TStream; const Path : string) : boolean;
    begin
      fLevelFiles.DeleteAll;
      fLevelPath := Path;
      try
        fTargetFile := aStream;
        result := {ReadLine(0, voidStr) and } ReadLevel(0);
      except
        result := false;
      end;
    end;

  procedure TLevelManager.Reset;
    var
      i : integer;
    begin
      for i := 0 to pred(fLevelFiles.Count) do
        TStream(fLevelFiles[i]).Seek(0, soFromBeginning);
    end;

  function TLevelManager.CreateFile(const path : string; level : integer) : boolean;
    begin
      try
        fLevelFiles.Insert(TFileStream.Create(path, fmCreate));
        result := true;
      except
        result := false;
      end;
    end;

  function TLevelManager.ReadLine(level : integer; var line : string) : boolean;
    var
      aux : string;
    begin
      try
        if DelphiStreamUtils.ReadLine(fTargetFile, aux)
          then
            begin
              line   := SkipSpaces(pchar(aux)); // copy(aux, 2*level + 1, length(aux)-2*level);
              result := true;
              inc(fLineNumber);
            end
          else result := false;
      except
        result := false;
      end;
    end;

  function TLevelManager.WriteLine(level : integer; const line : string) : boolean;
    begin
      try
        result := DelphiStreamUtils.WriteLine(Levels[level], line);
      except
        result := false;
      end;
    end;

  function TLevelManager.GetLevelPos(level : integer) : integer;
    begin
      if level < fLevelFiles.Count
        then result := Levels[level].Size
        else result := 0;
    end;

  function TLevelManager.ReadLevel(level : integer) : boolean;
    var
      lvStart : integer;
      lvEnd   : integer;
      curProp : string;
      nxtProp : string;
      error   : boolean;
    begin
      try
        if level = fLevelFiles.Count
          then result := CreateFile(fLevelPath + 'level_' + IntToStr(level) + '.txt', level)
          else result := true;
        if result and ReadLine(level, nxtProp)
          then
            if nxtProp <> EndMark
              then
                begin
                  repeat
                    curProp := nxtProp;
                    if ReadLine(level, nxtProp)
                      then
                        if nxtProp = BeginMark
                          then
                            begin
                              lvStart := GetLevelPos(level + 1);
                              if ReadLevel(level + 1)
                                then
                                  begin
                                    lvEnd := GetLevelPos(level + 1);
                                    if WriteLine(level, curProp + '[' + IntToStr(lvStart) + ',' + IntToStr(lvEnd) + ']')
                                      then
                                        if level > 0
                                          then error := not ReadLine(level, nxtProp)
                                          else error := false //error := not ReadLine(level, nxtProp) or (fTargetFile.Position <> fTargetFile.Size)
                                      else error := true;
                                  end
                                else error := true;
                            end
                          else error := not WriteLine(level, curProp)
                      else error := true;
                  until error or (nxtProp = EndMark) or (fTargetFile.Position = fTargetFile.Size);
                  result := not error;
                end
              else result := true
          else result := false;
      except
        result := false;
      end;
    end;

  function TLevelManager.GetLevel(index : integer) : TStream;
    begin
      result := TStream(fLevelFiles[index]);
    end;

  function TLevelManager.GetLevelCount : integer;
    begin
      result := fLevelFiles.Count;
    end;

  // TMultiLevelStream

  constructor TMultiLevelStream.Create(aStream : TStream; const Path : string);
    var
      i : integer;
    begin
      inherited Create;
      fLevelMan := TLevelManager.Create;
      if fLevelMan.CreateLevels(aStream, Path)
        then
          begin
            GetMem(fLevelBounds, (fLevelMan.LevelCount + 1)* sizeof(fLevelBounds[0]));
            FillChar(fLevelBounds^, (fLevelMan.LevelCount + 1) * sizeof(fLevelBounds[0]), 0);
            fLargestSize  := 0;
            fLargestLevel := fLevelMan[0];
            fLevelProps   := TCollection.Create(fLevelMan.LevelCount, rkBelonguer);
            for i := 0 to pred(fLevelMan.LevelCount) do
              begin
                fLevelProps.Insert(TStringList.Create);
                if fLargestSize < fLevelMan[i].Size
                  then
                    begin
                      fLargestSize  := fLevelMan[i].Size;
                      fLargestLevel := fLevelMan[i];                 
                    end;
              end;
            fLevelMan.Reset;
          end
        else
          begin
            fLevelMan.Free;
            fLevelMan := nil;
            raise EBlockLevelError.Create('Cannot create levels');
          end;
    end;

  destructor TMultiLevelStream.Destroy;
    begin
      fLevelMan.Free;
      fLevelProps.Free;
      if fLevelBounds <> nil
        then FreeMem(fLevelBounds);
      inherited;
    end;

  function GetNameValueFmt(const str : string) : string;
    var
      p : integer;
    begin
      p := 1;
      result := GetNextStringUpTo(str, p, ' ');
      SkipChars(str, p, Spaces);
      SkipChars(str, p, ['=']);
      SkipChars(str, p, Spaces);
      result := result + '=' + GetNextStringUpTo(str, p, #0);
    end;

  function TMultiLevelStream.ReadPropValue(const Name : string; var Prop : string; var eob : boolean) : boolean;
    var
      line   : string;
      aux    : string;
      Stream : TStream;
      p      : integer;
    begin
      Stream := fLevelMan[fCurLevel];
      if ReadLine(Stream, line)
        then
          begin
            if Name <> ''
              then
                begin
                  aux := Name + EqualSign;
                  if pos(aux, line) = 1
                    then
                      begin
                        Prop := copy(line, length(aux) + 1, length(line));
                        result := true;
                      end
                    else
                      begin
                        LogText('Forwarded Property: ' + Name + ' in level ' + IntToStr(fCurLevel));
                        TStringList(fLevelProps[fCurLevel]).Add(GetNameValueFmt(line));
                        Prop := '';
                        result := false;
                      end;
                end
              else
                begin
                  p := pos(EqualSign, line) + length(EqualSign);
                  Prop := copy(line, p, length(line) - p + 1);
                  result := true;
                end;
          end
        else
          begin
            result := false;
            Prop   := '';
          end;
      eob := Stream.Position >= LevelEnd;
    end;

  function TMultiLevelStream.ReadString(const Name, DefVal : string) : string;
    var
      CurLevel : TStringList;
      index    : integer;
      eob      : boolean;
    begin
      CurLevel := TStringList(fLevelProps[fCurLevel]);
      // Check if the prop was already read
      index := CurLevel.IndexOfName(Name);
      if index <> -1
        then result := CurLevel.Values[Name]
        else
          begin
            eob := false;
            while not eob and not ReadPropValue(Name, result, eob) do;
          end;
      if result = ''
        then
          begin
            result := DefVal;
            LogText('Missing Property: ' + Name + ' in Level ' + IntToStr(fCurLevel));
          end
        else
          if index <> -1
            then CurLevel.Delete(index);
    end;

{
  function TMultiLevelStream.ReadString(const Name, DefVal : string) : string;
    var
      CurLevel : TStringList;
      index    : integer;
    begin
      CurLevel := TStringList(fLevelProps[fCurLevel]);
      index    := CurLevel.IndexOfName(Name);
      result   := CurLevel.Values[Name];
      while (result = '') and ReadPropValue(Name, result) do;
      if result = ''
        then result := DefVal
        else
          if index <> -1
            then CurLevel.Delete(index);
    end;
}

  function TMultiLevelStream.ReadLevel(const Name, DefVal : string) : string;
    var
      aux : string;
      p   : integer;
    begin
      aux := ReadString(Name, DefVal);
      if aux <> DefVal
        then
          begin
            p := pos(BeginRange, aux);
            if p > 0
              then
                with fLevelBounds[fCurLevel + 1] do
                  begin
                    result := copy(aux, 1, p - 1);
                    inc(p);
                    LvStart := StrToInt(GetNextStringUpTo(aux, p, RangeSep));
                    inc(p);
                    LvEnd   := StrToInt(GetNextStringUpTo(aux, p, EndRange));
                  end
              else
                with fLevelBounds[fCurLevel + 1] do
                  begin
                    LvStart := 0;
                    LvStart := 0;
                    result  := aux;
                  end;
          end
        else result := DefVal;
    end;

  procedure TMultiLevelStream.EnterLevel;
    begin
      inc(fCurLevel);
      fLevelMan[fCurLevel].Position := LevelStart;
    end;

  procedure TMultiLevelStream.LeaveLevel;
    var
      List : TStringList;
    begin
      List := TStringList(fLevelProps[fCurLevel]);
      if List.Count > 0
        then
          begin
            LogText('Unused Properties in level ' + IntToStr(fCurLevel));
            List.SaveToStream(fLog);
            LogText('');
            List.Clear;
          end;
      dec(fCurLevel);
    end;

  procedure TMultiLevelStream.LogText(text : string);
    begin
      DelphiStreamUtils.WriteLine(fLog, text);
    end;

  function TMultiLevelStream.GetPosition : integer;
    begin
      result := fLargestLevel.Position;
    end;

  function TMultiLevelStream.GetSize : integer;
    begin
      result := fLargestSize;
    end;

  function TMultiLevelStream.GetLevelEnd : integer;
    begin
      result := fLevelBounds[fCurLevel].LvEnd;
    end;

  procedure TMultiLevelStream.SetLevelEnd(offset : integer);
    begin
      fLevelBounds[fCurLevel].LvEnd := offset;
    end;

  function TMultiLevelStream.GetLevelStart : integer;
    begin
      result := fLevelBounds[fCurLevel].LvStart;
    end;

  procedure TMultiLevelStream.SetLevelStart(offset : integer);
    begin
      fLevelBounds[fCurLevel].LvStart := offset;
    end;

end.
