unit RDOChannelCodec;

interface

  type
    TRDOChannelCodec =
      class
        private
          fAliases : TStringList;
          function AliasOf( aStr : string ) : string;
          function StringOf( aAlias : string ) : string;
        public
          constructor Create;
          destructor  Destroy; override;
        public
          function EncodeQuery( Query : string ) : string;
          function DecodeQuery( EncodedQuery : string ) : string;
      end;

implementation

  uses
    RDOProtocol, RDOUtils;

  type
    TAliasData =
      class
        private
          fRepeatCount : integer;
        public
          property RepeatCount : integer read fRepeatCount write fRepeatCount;
      end;

  // TAliasData

  // TRDOChannelCodec

  constructor TRDOChannelCodec.Create;
    begin
      inherited;
      fAliases := TStringList.Create
    end;

  destructor TRDOChannelCodec.Destroy;
    begin
      fAliases.Free;
      inherited
    end;

  function TRDOChannelCodec.AliasOf( aStr : string ) : string;
    begin

    end;

  function TRDOChannelCodec.EncodeQuery( Query : string ) : string;
    var

    begin
    end;

  function TRDOChannelCodec.DecodeQuery( EncodedQuery : string ) : string;
    var
      AliasIdPos   : integer;
      DecodedQuery : string;
    begin
      AliasIdPos := Pos( Query, AliasId );
      while AliasIdPos <> 0 do
        begin

        end
    end;

end.
