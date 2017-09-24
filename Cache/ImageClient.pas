unit ImageClient;

interface

  uses
    SysUtils, Classes;

  type
    TImageClient =
      class
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fName   : string;
          fWorld  : string;
          fSize   : integer;
          fStream : TStream;
          fLoaded : boolean;
        public
          function Recieve(var data; len : integer) : boolean;
        public
          property Name   : string  read fName write fName;
          property World  : string  read fWorld write fWorld;
          property Size   : integer read fSize write fSize;
          property Loaded : boolean read fLoaded write fLoaded;
          property Stream : TStream read fStream;
      end;

implementation

  // TImageClient

  constructor TImageClient.Create;
    begin
      inherited;
      fStream := TMemoryStream.Create;
    end;

  destructor TImageClient.Destroy;
    begin
      fStream.Free;
      inherited;
    end;

  function TImageClient.Recieve(var data; len : integer) : boolean;
    begin
      fStream.Write(data, len);
      result := fStream.Size >= fSize;
    end;

end.
 