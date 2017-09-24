unit FIFOIntf;

interface

  uses
    Windows;

  type
    IFIFO =
      interface['{7931C5E1-6515-11d3-B638-00400566F3E8}']
        function Write(const Data; Size : integer) : hResult; stdcall;
        function Read(out Data; Size : integer; out Actual : integer) : hResult; stdcall;
        function Peek(out Data; Size : integer; out Actual : integer) : hResult; stdcall;
        function Advance(Size : integer; out Actual : integer) : hResult; stdcall;
        function Clear : hResult; stdcall;
        function GetSize(out Size : integer) : integer; stdcall;
      end;

implementation

end.
