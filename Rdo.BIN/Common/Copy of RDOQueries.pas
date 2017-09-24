unit RDOQueries;

interface

  uses
    SysUtils, Classes, SocketComp;

  type
    TRDOQueryKind     = (qkNone, qkAnswer, qkGeID, qkSetProp, qkGetProp, qkCallRet, qkCallNoRet, qkError);
    TRDOParamKind     = (pkInteger, pkShortString, pkString, pkSingle, pkDouble, pkEmpty, pkOutParam, pkBuffer);
    TRDOQueryPriority = (qpNormal, qpAboveNormal, qpBelowNormal, qpHighest, qpIdle, qpLowest, qpTimeCritical);

  type
    ERDOQueryError = class(Exception);

  type
    TQueryStream = class;
    TRDOQuery    = class;

    PQueryStreamData = ^TQueryStreamData;
    TQueryStreamData =
      record
        Size : integer;
        Data : array[0..0] of char;
      end;

    TQueryStream =
      class(TStream)
        public
          destructor Destroy; override;
        private
          fData : PQueryStreamData;
          fPos  : integer;
        public
          procedure Clear;
          function  Receive(Socket : TCustomWinSocket) : boolean;
          function  Send(Socket : TCustomWinSocket) : boolean;
          function  Read(var buffer; count : longint): longint;     override;
          function  Write(const buffer; count : longint): longint;  override;
          function  Seek(offset : longint; origin : word): longint; override;
        protected
          function  GetSize : integer;
          procedure SetSize(aSize : integer); override;
        public
          property Position : longint read fPos    write fPos;
          property Size     : longint read GetSize write SetSize;
      end;

    TRDOQuery =
      class
        public
          constructor Create(aQueryKind : TRDOQueryKind; anId : word);
          constructor Read(Stream : TStream);
          procedure   Write(Stream : TStream);
          destructor  Destroy; override;
        private
          fName       : string;
          fId         : word;
          fQKind      : TRDOQueryKind;
          fObject     : integer;
          fPriority   : TRDOQueryPriority;
          fParamCount : byte;
          fParamSize  : integer;
          fParams     : pchar;
        public
          property Id         : word          read fId     write fId;
          property QKind      : TRDOQueryKind read fQKind  write fQKind;
          property Name       : string        read fName   write fName;
          property ObjId      : integer       read fObject write fObject;
          property ParamCount : byte          read fParamCount;
          property Priority   : TRDOQueryPriority read fPriority write fPriority;
        private
          procedure SetParamSize(aSize : integer);
        protected
          property ParamSize : integer read fParamSize write SetParamSize;
        private
          procedure PushString(const str : string);
          procedure PushValue(const value; k : TRDOParamKind; size : integer);
        public
          procedure Clear;
          procedure PushParam(const value : variant);
          procedure PushParamRef;
          procedure PushPtr(ptr : pointer; size : integer);
        protected
          function  PopInteger(var idx : integer) : integer;
          function  PopShortString(var idx : integer) : string;
          function  PopString(var idx : integer) : string;
          function  PopSingle(var idx : integer) : single;
          function  PopDouble(var idx : integer) : double;
          function  PopPtr(var idx : integer) : variant; virtual;
        public
          {$IFDEF AUTO}
          function  PopParam(var idx : integer) : OleVariant;
          {$ELSE}
          function  PopParam(var idx : integer) : variant;
          {$ENDIF}
          function  ToStr : string;
      end;

  function QueryPriorityToThreadPriority(prior : TRDOQueryPriority) : integer;
  function ThreadPriorityToQueryPriority(prior : integer) : TRDOQueryPriority;

implementation

  uses
    Windows, RDOVariantUtils;

  function QueryPriorityToThreadPriority(prior : TRDOQueryPriority) : integer;
    begin
      case prior of
        qpLowest :
          result := THREAD_PRIORITY_LOWEST;
        qpBelowNormal :
          result := THREAD_PRIORITY_BELOW_NORMAL;
        qpNormal :
          result := THREAD_PRIORITY_NORMAL;
        qpHighest :
          result := THREAD_PRIORITY_HIGHEST;
        qpAboveNormal :
          result := THREAD_PRIORITY_ABOVE_NORMAL;
        qpTimeCritical :
          result := THREAD_PRIORITY_TIME_CRITICAL;
        qpIdle :
          result := THREAD_PRIORITY_IDLE;
        else
          result := THREAD_PRIORITY_NORMAL;
      end
    end;

  function ThreadPriorityToQueryPriority(prior : integer) : TRDOQueryPriority;
    begin
      case prior of
        THREAD_PRIORITY_LOWEST:
          result := qpLowest;
        THREAD_PRIORITY_BELOW_NORMAL:
          result := qpBelowNormal;
        THREAD_PRIORITY_NORMAL:
          result := qpNormal;
        THREAD_PRIORITY_HIGHEST:
          result := qpHighest;
        THREAD_PRIORITY_ABOVE_NORMAL:
          result := qpAboveNormal;
        THREAD_PRIORITY_TIME_CRITICAL:
          result := qpTimeCritical;
        THREAD_PRIORITY_IDLE:
          result := qpIdle;
        else
          result := qpNormal;
      end
    end;

  procedure WriteStrOnStr(var dest : string; src : string; var pos : integer; fmsz : byte);
    var
      len : integer;
    begin
      len := length(src);
      move(len, dest[pos], fmsz);
      inc(pos, fmsz);
      if len > 0
        then move(src[1], dest[pos], len);
    end;


  // TRDOQueryStream

  destructor TQueryStream.Destroy;
    begin
      ReallocMem(fData, 0);
      inherited;
    end;

  procedure TQueryStream.Clear;
    begin
      SetSize(0);
      fPos := 0;
    end;

  function TQueryStream.Receive(Socket : TCustomWinSocket) : boolean;
    var
      cnt : integer;
      sz  : integer;
    begin
      if Size = 0
        then
          begin
            if Socket.ReceiveLength > sizeof(fData.Size)
              then
                begin
                  Socket.ReceiveBuf(sz, sizeof(sz));
                  if sz <= 0
                    then raise ERDOQueryError.Create('Invalid size for query')
                    else SetSize(sz);
                end;
            fPos := 0;
          end;
      sz := Size;
      if sz > 0
        then
          begin
            cnt := Socket.ReceiveBuf(fData.Data[fPos], sz - fPos);
            inc(fPos, cnt);
          end;
      result := (sz > 0) and (fPos = sz);
    end;

  function TQueryStream.Send(Socket : TCustomWinSocket) : boolean;
    begin
      try
        if fData <> nil
          then
            begin
              Socket.SendBuf(fData^, Size + sizeof(fData.Size));
              result := true;
            end
          else result := false;
      except
        result := false;
      end;
    end;

  function TQueryStream.Read(var buffer; count : longint) : longint;
    var
      sz : integer;
    begin
      sz := Size;
      if fPos + count > sz
        then result := sz - fPos
        else result := count;
      if result > 0
        then
          begin
            move(fData.Data[fPos], buffer, result);
            inc(fPos, result);
          end
        else result := 0;
    end;

  function TQueryStream.Write(const buffer; count : longint) : longint;
    var
      sz : integer;
    begin
      sz := Size;
      try
        if fPos + count > sz
          then Size := fPos + count;
        move(buffer, fData.Data[fPos], count);
        inc(fPos, count);
        result := count;
      except
        result := 0;
      end;
    end;

  function TQueryStream.Seek(offset : longint; origin : word): longint;
    var
      sz : integer;
    begin
      sz := Size;
      case origin of
        soFromBeginning	:
          fPos := offset;
        soFromCurrent :
          inc(fPos, offset);
        soFromEnd :
          fPos := sz - offset;
      end;
      if fPos < 0
        then fPos := 0
        else
          if fPos > sz
            then fPos := sz;
      result := fPos;
    end;

  function TQueryStream.GetSize : integer;
    begin
      if fData <> nil
        then result := fData.Size
        else result := 0;
    end;

  procedure TQueryStream.SetSize(aSize : integer);
    begin
      ReallocMem(fData, aSize + sizeof(aSize));
      if fData <> nil
        then fData.Size := aSize;
    end;


  // TRDOQuery

  constructor TRDOQuery.Create(aQueryKind : TRDOQueryKind; anId : word);
    begin
      inherited Create;
      fId       := anId;
      fQKind    := aQueryKind;
      fPriority := qpNormal;
    end;

  constructor TRDOQuery.Read(Stream : TStream);
    var
      len : integer;
    begin
      Stream.Read(fId, sizeof(fId));
      Stream.Read(fPriority, sizeof(fPriority));
      Stream.Read(fQKind, sizeof(fQKind));
      case fQKind of
        qkError, qkAnswer :
          begin
            Stream.Read(fParamCount, sizeof(fParamCount));
            len := Stream.Size - Stream.Position;
            ParamSize := len;
            if len > 0
              then Stream.Read(fParams[0], len)
              else fParams := nil;
          end;
        qkGeID :
          begin
            fillchar(len, sizeof(len), 0);
            Stream.Read(len, 1);
            if len > 0
              then
                begin
                  SetLength(fName, len);
                  Stream.Read(fName[1], len);
                end
              else fName := '';
          end;
        qkSetProp, qkGetProp, qkCallRet, qkCallNoRet :
          begin
            Stream.Read(fObject, sizeof(fObject));
            fillchar(len, sizeof(len), 0);
            Stream.Read(len, 1);
            if len > 0
              then
                begin
                  SetLength(fName, len);
                  Stream.Read(fName[1], len);
                end
              else fName := '';
            Stream.Read(fParamCount, sizeof(fParamCount));
            len := Stream.Size - Stream.Position;
            if len > 0
              then
                begin
                  ParamSize := len;
                  Stream.Read(fParams[0], len);
                end
              else fParams := nil;
          end;
        else raise ERDOQueryError.Create('Unknown query kind');
      end;
    end;

  procedure TRDOQuery.Write(Stream : TStream);
    var
      pSz : integer;
      len : integer;
    begin
      Stream.Write(fId, sizeof(fId));
      Stream.Write(fPriority, sizeof(fPriority));
      Stream.Write(fQKind, sizeof(fQKind));
      pSz := ParamSize;
      case fQKind of
        qkError, qkAnswer :
          begin
            Stream.Write(fParamCount, sizeof(fParamCount));
            if pSz > 0
              then Stream.Write(fParams[0], pSz);
          end;
        qkGeID :
          begin
            len := length(fName);
            Stream.Write(len, 1); // lo-byte of length
            if len > 0
              then Stream.Write(fName[1], len);
          end;
        qkSetProp, qkGetProp, qkCallRet, qkCallNoRet :
          begin
            Stream.Write(fObject, sizeof(fObject));
            len := length(fName);
            Stream.Write(len, 1); // lo-byte of length
            if len > 0
              then Stream.Write(fName[1], len);
            Stream.Write(fParamCount, sizeof(fParamCount));
            if pSz > 0
              then Stream.Write(fParams[0], pSz);
          end;
      end;
    end;

  destructor TRDOQuery.Destroy;
    begin
      Clear;
      inherited;
    end;

  procedure TRDOQuery.SetParamSize(aSize : integer);
    begin
      if aSize < 0
        then aSize := 0;
      fParamSize := aSize;
      ReallocMem(fParams, aSize);
    end;

  procedure TRDOQuery.PushString(const str : string);
    var
      strln : integer;
      len   : integer;
      fmsz  : byte;
      k     : TRDOParamKind;
    begin
      len   := ParamSize;
      strln := length(str);
      if strln < high(byte)
        then
          begin
            k    := pkShortString;
            fmsz := sizeof(byte);
          end
        else
          begin
            k    := pkString;
            fmsz := sizeof(integer);
          end;
      ParamSize := len + sizeof(k) + fmsz + strln;
      move(k, fParams[len], sizeof(k));
      inc(len, sizeof(k));
      move(strln, fParams[len], fmsz);
      inc(len, fmsz);
      if strln > 0
        then move(str[1], fParams[len], strln);
    end;

  procedure TRDOQuery.PushValue(const value; k : TRDOParamKind; size : integer);
    var
      len : integer;
    begin
      len := ParamSize;
      ParamSize := len + sizeof(k) + size;
      move(k, fParams[len], sizeof(k));
      inc(len, sizeof(k));
      move(value, fParams[len], size);
    end;

  procedure TRDOQuery.Clear;
    begin
      fName       := '';
      fParamCount := 0;
      fObject     := 0;
      fQKind      := qkNone;
      ParamSize   := 0;
    end;

  procedure TRDOQuery.PushParam(const value : variant);
    var
      vt  : word;
      str : string;
      i   : integer;
      s   : single;
      d   : double;
      mp  : TMarshalPtr;
    begin
      inc(fParamCount);
      vt := TVarData(value).VType and varTypeMask;
      case vt of
        varOleStr, varString:
          begin
            str := value;
            PushString(str);
          end;
        varBoolean :
          begin
            if value
              then i := 1
              else i := 0;
            PushValue(i, pkInteger, sizeof(integer));
          end;
        varSmallint, varInteger, varError, varByte:
          begin
            i := value;
            PushValue(i, pkInteger, sizeof(integer));
          end;
        varSingle:
          begin
            s := value;
            PushValue(s, pkSingle, sizeof(single));
          end;
        varDouble, varDate, varCurrency:
          begin
            d := value;
            PushValue(d, pkDouble, sizeof(double));
          end;
        varVariant, varEmpty:
          begin
            i := 0;
            PushValue(i, pkEmpty, 0);
          end;
        varPointer, varByRef, varUnknown:
          begin
            mp := GetMarshalPtr(value);
            PushPtr(mp.ptr, mp.size);
          end;
        else raise ERDOQueryError.Create('Illegal type found.');
      end;
    end;

  procedure TRDOQuery.PushParamRef;
    var
      useless : integer;
    begin
      inc(fParamCount);
      PushValue(useless, pkOutParam, 0);
    end;

  procedure TRDOQuery.PushPtr(ptr : pointer; size : integer);
    var
      sz : integer;
    begin
      inc(fParamCount);
      PushValue(size, pkBuffer, sizeof(size));
      sz := ParamSize;
      ParamSize := size + sz;
      move(ptr^, fParams[sz], size);
    end;

  function TRDOQuery.PopInteger(var idx : integer) : integer;
    begin
      result := 0;
      if idx + sizeof(result) <= ParamSize
        then move(fParams[idx], result, sizeof(result))
        else raise ERDOQueryError.Create('Out of range parameter');
      inc(idx, sizeof(result));
    end;

  function TRDOQuery.PopShortString(var idx : integer) : string;
    var
      sz  : byte;
      len : integer;
    begin
      len := ParamSize;
      if idx <= len
        then
          begin
            sz := byte(fParams[idx]);
            inc(idx);
            if idx + sz <= len
              then
                if sz > 0
                  then
                    begin
                      SetLength(result, sz);
                      move(fParams[idx], result[1], sz);
                      inc(idx, sz);
                    end
                  else result := ''
              else
                begin
                  result := '';
                  raise ERDOQueryError.Create('Out of range parameter');
                end;
          end
        else result := '';
    end;

  function TRDOQuery.PopString(var idx : integer) : string;
    var
      sz : integer;
    begin
      sz := PopInteger(idx);
      if idx + sz <= ParamSize
        then
          if sz > 0
            then
              begin
                SetLength(result, sz);
                move(fParams[idx], result[1], sz);
                inc(idx, sz);
              end
            else result := ''
        else
          begin
            raise ERDOQueryError.Create('Out of range parameter');
            result := '';
          end;
    end;

  function TRDOQuery.PopSingle(var idx : integer) : single;
    begin
      result := 0;
      if idx + sizeof(result) <= ParamSize
        then move(fParams[idx], result, sizeof(result))
        else raise ERDOQueryError.Create('Out of range parameter');
      inc(idx, sizeof(result));
    end;

  function TRDOQuery.PopDouble(var idx : integer) : double;
    begin
      result := 0;
      if idx + sizeof(result) <= ParamSize
        then move(fParams[idx], result, sizeof(result))
        else raise ERDOQueryError.Create('Out of range parameter');
      inc(idx, sizeof(result));
    end;

  function TRDOQuery.PopPtr(var idx : integer) : variant;
    var
      size : integer;
      ptr  : pointer;
    begin
      size := PopInteger(idx);
      if size > 0
        then
          if fQKind = qkAnswer
            then
              begin
                GetMem(ptr, size);
                move(fParams[idx], ptr^, size);
                inc(idx, size);
              end
            else ptr := fParams + idx
        else ptr := nil;
      result := integer(ptr);
      inc(idx, size);
    end;

  {$IFDEF AUTO}
  function TRDOQuery.PopParam(var idx : integer) : OleVariant;
  {$ELSE}
  function TRDOQuery.PopParam(var idx : integer) : variant;
  {$ENDIF}
    var
      k   : TRDOParamKind;
      len : integer;
    begin
      len := ParamSize;
      if idx + sizeof(k) <= len
        then
          begin
            move(fParams[idx], k, sizeof(k));
            inc(idx, sizeof(k));
            case k of
              pkInteger :
                result := PopInteger(idx);
              pkShortString :
                result := PopShortString(idx);
              pkString :
                result := PopString(idx);
              pkSingle :
                result := PopSingle(idx);
              pkDouble :
                result := PopDouble(idx);
              pkOutParam :
                TVarData(result).VType := varVariant;
              pkBuffer :
                result := PopPtr(idx);
              else
                result := integer(0);
            end;
          end
        else result := integer(0);
    end;

  function TRDOQuery.ToStr : string;
    function RenderParams : string;
      var
        i   : integer;
        idx : integer;
        v   : variant;
      begin
        result := '';
        idx    := 0;
        for i := 1 to fParamCount do
          begin
            v := PopParam(idx);
            if TVarData(v).VType and varTypeMask <> varVariant
              then result := result + VarToStr(v)
              else result := result + '?';
            if i < fParamCount
              then result := result + ', ';
          end;
        if result <> ''
          then result := '(' + result + ')';
      end;
    begin
      case fQKind of
        qkAnswer :
          result := 'Answer' + RenderParams;
        qkError :
          result := 'Error' + RenderParams;
        qkGeID :
          result := 'IdOf(' + fName + ')';
        qkSetProp :
          result := IntToStr(fObject) + '.Set' + fName + RenderParams;
        qkGetProp :
          result := IntToStr(fObject) + '.Get' + fName;
        qkCallRet, qkCallNoRet :
          result := IntToStr(fObject) + '.' + fName + RenderParams;
      end;
      result := 'Query#' + IntToStr(fId) + ' = ' + result;
    end;

end.
