unit VCLUtils;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise.

interface

  uses
    Windows, Forms, Classes, Controls, WinUtils;

  function  ParentRect( aControl : TControl ) : TRect; // Returns the rectangle of this control in parent's coordinates
  function  ScreenRect( aControl : TControl ) : TRect; // Returns the rectangle this control is using in screen coordinates

  procedure AvoidResizing( aForm : TForm );            // Disable resizing of this form
  procedure AllowResizing( aForm : TForm );            // Allow resizing of this form

  procedure HideFromTaskbar;                           // Remove the application from the taskbar
  procedure ShowOnTaskbar;                             // Show the application again in the taskbar

  function UniqueComponentName( Owner : TComponent; const NamePrefix : string ) : string;
  // eg: UniqueComponentName( Owner, 'Tool' ) = 'Tool3' if 'Tool1' and 'Tool2' already exist in Owner

implementation

  uses
    SysUtils, StrUtils;
    
  function UniqueComponentName( Owner : TComponent; const NamePrefix : string ) : string;
    var
      i, FirstFreeIndx    : integer;
    begin
      with Owner do
        begin
          FirstFreeIndx := 1;
          i             := 0;
          while i < ComponentCount do
            if IndxOf( Components[i].Name, NamePrefix ) = FirstFreeIndx
              then
                begin
                  inc( FirstFreeIndx );
                  i := 0; // Start over
                end
              else inc( i );
        end;
      Result := NamePrefix + IntToStr( FirstFreeIndx );
    end;

  procedure HideFromTaskbar;
    begin
      HideWindow( Application.Handle );
    end;

  procedure ShowOnTaskbar;
    begin
      ShowDontActivateWindow( Application.Handle );
    end;

  function ParentRect( aControl : TControl ) : TRect;
    begin
      with aControl do
        begin
          Result := ClientRect;
          if Assigned( Parent )
            then
              begin
                with ClientOrigin do
                  OffsetRect( Result, x, y );
                with Parent.ClientOrigin do
                  OffsetRect( Result, -x, -y );
              end;
        end;
    end;

  function ScreenRect( aControl : TControl ) : TRect;
    begin
      with aControl, ClientOrigin do
        begin
          Result := ClientRect;
          OffsetRect( Result, x, y );
        end;
    end;

  procedure AvoidResizing( aForm : TForm );
    begin
      SetWindowSizeable( aForm.Handle, false );
    end;

  procedure AllowResizing( aForm : TForm );
    begin
      SetWindowSizeable( aForm.Handle, true  );
    end;

end.
