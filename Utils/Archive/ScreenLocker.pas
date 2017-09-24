unit ScreenLocker;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise.

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

  type
    TFormLock =
      class( TForm )
      protected
        procedure WMEraseBkgnd( var Message : TWMEraseBkgnd );   message WM_ERASEBKGND;
        procedure CreateParams( var Params : TCreateParams );    override;
      end;

  type
    TScreenLocker =
      class
        procedure Lock;
        procedure Unlock;

      private
        FormLock : TFormLock;
      end;

implementation

  {$R *.DFM}

  procedure TFormLock.CreateParams( var Params : TCreateParams );
    begin
      inherited;
      with Params do
        WndParent := 0;
    end;

  procedure TFormLock.WMEraseBkgnd( var Message : TWMEraseBkgnd );
    begin
    end;

  procedure TScreenLocker.Lock;
    const
      ShowFlags = SWP_NOACTIVATE + SWP_SHOWWINDOW;
    begin
      FormLock := TFormLock.Create( nil );
      with FormLock do
        SetWindowPos( Handle, HWND_TOPMOST, 0, 0, Screen.Width, Screen.Height, ShowFlags );
    end;

  procedure TScreenLocker.Unlock;
    begin
      FormLock.Free;
    end;

end.
