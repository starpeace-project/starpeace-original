unit FiveErrorHandler;

interface

implementation

  uses
    Windows;

  procedure _ExceptProc(ExceptObject: TObject; ExceptAddr: Pointer);
    begin
      // Windows.MessageBox(0, '1', '1', 1);
    end;

  procedure _ErrorProc(ErrorCode: Integer; ErrorAddr: Pointer);
    begin
      // Windows.MessageBox(0, '2', '2', 1);
    end;

  procedure _AssertErrorHandler(const Message, Filename: string; LineNumber: Integer; ErrorAddr: Pointer);
    begin
      // Windows.MessageBox(0, '3', '3', 1);
    end;

  procedure _AbstractErrorHandler;
    begin
      // Windows.MessageBox(0, '4', '4', 1);
    end;


initialization 

  ExceptProc := @_ExceptProc;
  ErrorProc  := @_ErrorProc;
  AssertErrorProc := @_AssertErrorHandler;
  AbstractErrorProc := @_AbstractErrorHandler;

end.


