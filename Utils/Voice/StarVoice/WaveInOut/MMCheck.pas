unit MMCheck;

interface

  uses
    Windows, MMSystem,
    SysUtils;

  type
    EMultimedia =
      class(Exception)
        public
          constructor Create(aResult : hResult);
      end;

  procedure TryMM(aResult : HResult);

implementation

  constructor EMultimedia.Create(aResult : hResult);
    begin
      inherited Create('Multimedia system is busy');
    end;

  procedure TryMM(aResult : HResult);
    begin
      if aResult <> MMSYSERR_NOERROR
        then raise EMultimedia.Create(aResult);
    end;

end.
                                         
