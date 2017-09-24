unit RDOVariantUtils;

interface

  const
    varPointer = $201;

  type
    TMarshalPtr =
      packed record
        ptr  : pointer;
        size : integer;
      end;

  procedure MarshalReturnPtr(buffer : pointer; size : integer; var v : variant);
  function  MarshalPtr(buffer : pointer; size : integer) : variant;
  function  UnMarshalPtr(const v : variant) : pointer;
  function  GetMarshalPtr(const v : variant) : TMarshalPtr;

  function  IsVarParam(const v : variant) : boolean;
  function  IsPtrParam(const v : variant) : boolean;

implementation

  procedure MarshalReturnPtr(buffer : pointer; size : integer; var v : variant);
    var
      aux : double;
    begin
      TVarData(v).vType     := varPointer;
      TMarshalPtr(aux).ptr  := buffer;
      TMarshalPtr(aux).size := size;
      TVarData(v).vDouble   := aux;
    end;

  function MarshalPtr(buffer : pointer; size : integer) : variant;
    var
      aux : double;
    begin
      TVarData(result).vType   := varPointer;
      TMarshalPtr(aux).ptr     := buffer;
      TMarshalPtr(aux).size    := size;
      TVarData(result).vDouble := aux;
    end;

  function UnMarshalPtr(const v : variant) : pointer;
    begin
      result := TVarData(v).vPointer;
    end;

  function GetMarshalPtr(const v : variant) : TMarshalPtr;
    begin
      result := TMarshalPtr(TVarData(v).vDouble);
    end;

  function IsVarParam(const v : variant) : boolean;
    var
      aux : PVariant;
    begin
      if TVarData(v).VType and varTypeMask = varVariant
        then
          begin
            aux := TVarData(v).vPointer;
            result := (aux = nil) or (TVarData(aux^).VType <> varPointer);
          end
        else result := false;
    end;

  function IsPtrParam(const v : variant) : boolean;
    var
      aux : PVariant;
    begin
      if TVarData(v).VType and varTypeMask = varVariant
        then
          begin
            aux := TVarData(v).vPointer;
            result := (aux <> nil) or (TVarData(aux^).VType = varPointer);
          end
        else result := false;
    end;

end.
