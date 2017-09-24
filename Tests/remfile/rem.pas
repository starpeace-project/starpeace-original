unit rem;

interface

uses
  ComObj, ActiveX, remfile_TLB, StdVcl;

type
  TFS = class(TAutoObject, IFS)
  protected
    procedure Remove(const path: WideString); safecall;
    function Get_LastError: Integer; safecall;
    { Protected declarations }
  private
    fLastError : integer;
  end;

implementation

  uses ComServ, SysUtils;

  procedure TFS.Remove(const path: WideString);
    begin
      if SysUtils.DeleteFile(path)
        then fLastError := 0
        else fLastError := 1;
    end;


  function TFS.Get_LastError: Integer;
    begin
      result := fLastError;
    end;

initialization

  TAutoObjectFactory.Create(ComServer, TFS, Class_FS,
    ciMultiInstance, tmApartment);

end.
