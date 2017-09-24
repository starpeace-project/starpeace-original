unit BufferProviders;

interface

  uses
    Buffer;
    
  type
    CBufferProvider = class of TBufferProvider;
    TBufferProvider =
      class
        function CreateBitmap : TBuffer; virtual; abstract; 
      end;

  function  GetProvider( id : integer ) : TBufferProvider;
  procedure RegisterProvider( id : integer; Provider : CBufferProvider );

implementation

  function  GetProvider( id : integer ) : TBufferProvider;
    begin
      Result := nil;
      //
    end;

  procedure RegisterProvider( id : integer; Provider : CBufferProvider );
    begin
    end;

end.
