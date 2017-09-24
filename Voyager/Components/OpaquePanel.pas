unit OpaquePanel;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

  type
    TOpaquePanel =
      class(TPanel)
        private
          procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
      end;

  procedure Register;

implementation

  // TOpaquePanel.

  procedure TOpaquePanel.WMEraseBkgnd(var Message: TMessage);
    begin
      Message.Result := 1;
    end;

  procedure Register;
    begin
      RegisterComponents('Five', [TOpaquePanel]);
    end;

end.
