unit DirectDrawUtils;

interface

  uses
    Windows;

  procedure InitRecord(var aRecord; Size : dword);

implementation

  procedure InitRecord(var aRecord; Size : dword);
    var
      dwSize : dword absolute aRecord;
    begin
      ZeroMemory( @aRecord, Size );
      dwSize := Size;
    end;

end.
