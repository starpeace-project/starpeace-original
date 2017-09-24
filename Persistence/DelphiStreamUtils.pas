unit DelphiStreamUtils;

interface

  uses
    Classes, SysUtils;

  function ReadLine(const Stream : TStream; var line : string) : boolean;
  function WriteLine(const Stream : TStream; const line : string) : boolean;
  function ReadString(const Stream : TStream; var str : string) : boolean;
  function ReadStringUpTo(const Stream : TStream; var str : string; ch, marker : char) : boolean;
  function WriteString(const Stream : TStream; const str : string) : boolean;
  function StreamToText(const Stream : TStream; const path : string) : boolean;
  function StreamToHtmlText(const Stream : TStream; const path : string) : boolean;
  function StreamToStrings(const Stream : TStream; const Strings : TStrings) : boolean;
  function StringsToStream(const Strings : TStrings; const Stream : TStream) : boolean;
  function CopyStream(const Source, Dest : TStream) : boolean;
  function CopyTextStream(const Source, Dest : TStream; const sep : string) : boolean;

implementation

  uses
    StrUtils;

  const
    BufferSize    = 1024;
    CharBlockSize = 80;
    EOLN : word   = $0A0D;
    NoIndex       = -1;

  function ReadLine(const Stream : TStream; var line : string) : boolean;
    var
      position : integer;
      size     : integer;
      index    : integer;
      eol      : boolean;
      len      : integer;
      aux      : pchar;
    begin
      try
        position := Stream.Position;
        size     := Stream.Size;
        result   := position < size;
        if result
          then
            begin
              len := CharBlockSize;
              SetLength(line, len);
              aux   := pchar(line);
              index := 0;
              repeat
                if index = len
                  then
                    begin
                      inc(len, CharBlockSize);
                      SetLength(line, len);
                      aux := pchar(line);
                    end;
                Stream.Read(aux[index], sizeof(char));
                inc(position);
                eol := (index > 0) and ((aux[index - 1] = #$D) and (aux[index] = #$A) or (aux[index - 1] = #$A) and (aux[index] = #$D));
                inc(index);
              until (position = size) or eol;
              if eol then dec(index, 2);
              SetLength(line, index);
            end
          else line := '';
      except
        line   := '';
        result := false;
      end;
    end;

  function WriteLine(const Stream : TStream; const line : string) : boolean;
    begin
      try
        if line <> ''
          then Stream.Write(line[1], length(line));
        Stream.Write(EOLN, sizeof(EOLN));
        result := true;
      except
        result := false;
      end;
    end;

  function ReadString(const Stream : TStream; var str : string) : boolean;
    var
      len : integer;
    begin
      try
        Stream.Read(len, sizeof(len));
        if len > 0
          then
            begin
              SetLength(str, len);
              Stream.Read(str[1], len);
            end
          else str := '';
        result := true;
      except
        result := false;
      end;
    end;

  function CharPos(buffer : PChar; ch, mrk : char; var MrkCount : integer) : integer;
    const
      Mask : integer = 1;
    var
      index    : integer;
      CurChar  : char;
    begin
      index    := 0;
      CurChar  := buffer[index];
      while (CurChar <> #0) and (CurChar <> ch) or (MrkCount and Mask = 1) do
        begin
          if CurChar = mrk
            then inc(MrkCount);
          inc(index);
          CurChar := buffer[index];
        end;
      if CurChar <> #0
        then result := index
        else result := NoIndex;
    end;

  function ReadStringUpTo(const Stream : TStream; var str : string; ch, marker : char) : boolean;
    const
      BufferSize = 512;
    var
      buffer   : array[0..BufferSize] of char;
      save     : integer;
      flag     : boolean;
      count    : integer;
      index    : integer;
      epos     : integer;
      MrkCount : integer;
    begin
      save     := Stream.Position;
      index    := save;
      MrkCount := 0;
      repeat
        count := Stream.Read(buffer, BufferSize);
        buffer[count] := #0;
        epos := CharPos(buffer, ch, marker, MrkCount);
        flag := epos <> NoIndex;
        if flag
          then inc(index, epos + 1)
          else inc(index, count);
      until flag or (count < BufferSize);
      count := index - save;
      result := count = 0;
      if not result
        then
          begin
            Stream.Seek(save, soFromBeginning);
            SetLength(str, count);
            Stream.Read(str[1], count);
          end
        else
          str := '';
    end;

  function WriteString(const Stream : TStream; const str : string) : boolean;
    var
      len : integer;
    begin
      try
        len := length(str);
        Stream.Write(len, sizeof(len));
        if len > 0 then Stream.Write(str[1], len);
        result := true;
      except
        result := false;
      end;
    end;

  function StreamToText(const Stream : TStream; const path : string) : boolean;
    var
      aTextFile : TextFile;
      SavePos   : integer;
      aux       : string;
    begin
      try
        SavePos := Stream.Position;
        AssignFile(aTextFile, path);
        try
          Stream.Seek(0, 0);
          ReWrite(aTextFile);
          while ReadLine(Stream, aux) do
            WriteLn(aTextFile, aux);
        finally
          CloseFile(aTextFile);
        end;
        result := true;
        Stream.Position := SavePos;
      except
        result := true;
      end;
    end;

  function StreamToHtmlText(const Stream : TStream; const path : string) : boolean;
    var
      aTextFile : TextFile;
      SavePos   : integer;
      aux       : string;
    begin
      try
        SavePos := Stream.Position;
        AssignFile(aTextFile, path);
        try
          ReWrite(aTextFile);
          aux := '<body bgcolor = "white">';
          WriteLn(aTextFile, aux);
          aux := '<p>';
          WriteLn(aTextFile, aux);
          while ReadLine(Stream, aux) do
            begin
              WriteLn(aTextFile, aux);
              aux := '</p>' + #$D#$A + '<p>';
              WriteLn(aTextFile, aux);
            end;
          aux := '</p>';
          WriteLn(aTextFile, aux);
        finally
          CloseFile(aTextFile);
        end;
        result := true;
        Stream.Position := SavePos;
      except
        result := true;
      end;
    end;

  function StreamToStrings(const Stream : TStream; const Strings : TStrings) : boolean;
    var
      aux     : string;
    begin
      try
        while ReadLine(Stream, aux) do
          Strings.Add(aux);
        result := true;
      except
        result := true;
      end;
    end;

  function StringsToStream(const Strings : TStrings; const Stream : TStream) : boolean;
    var
      index : integer;
    begin
      try
        for index := 0 to pred(Strings.Count) do
          WriteLine(Stream, Strings[index]);
        result := true;
      except
        result := false;
      end;
    end;

  function CopyStream(const Source, Dest : TStream) : boolean;
    const
      CopyBufferSize = 1024;
    var
      buffer : array[1..CopyBufferSize] of byte;
      count  : integer;
    begin
      try
        repeat
          count := Source.Read(buffer, CopyBufferSize);
          Dest.Write(buffer, count);
        until count < CopyBufferSize;
        result := true;
      except
        result := false;
      end;
    end;

  function CopyTextStream(const Source, Dest : TStream; const sep : string) : boolean;
    var
      size : integer;
      line : string;
    begin
      try
        size := Source.Size;
        while Source.Position < size do
          begin
            ReadLine(Source, line);
            if line <> ''
              then line := line + sep;
            WriteLine(Dest, line);
          end;
        result := true;
      except
        result := false;
      end;
    end;

end.
