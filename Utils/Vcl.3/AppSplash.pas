unit AppSplash;

// To use this splash, you must substitute your normal initialization code, should look like this:
//
//  1 begin
//  1   Application.Initialize;
//  1   Application.Title := 'Foobar application';
//  1   Application.CreateForm(TForm1, Form1);
//  1   Application.CreateForm(TForm2, Form3);
//  1   Application.CreateForm(TForm3, Form3);
//  1   Application.Run;
//  1 end.
//
// by this:
//
//  2 uses (...)
//  2   Buffer, AppSplash;
//  2
//  2 procedure AppInit;
//  2   begin
//  2     Application.Title := 'Foobar application';
//  2     Application.CreateForm(TForm1, Form1);
//  2     Application.CreateForm(TForm2, Form3);
//  2     Application.CreateForm(TForm3, Form3);
//  2   end;
//  2
//  2 begin
//  2   ShowSplash( LoadBitmapFile( 'Splash.bmp' ), AppInit );
//  2   Application.Run
//  2 end.
//
//  For the bitmap parameter use your choice of:
//  > LoadBitmapFile( const Filename : string );
//  > LoadBitmapFromStream( const Stream : TStream );
//  > LoadBitmapFromResource( Instance : THandle; const ResourceName : string );
//  > LoadBitmapFromResourceID( Instance : THandle; ResourceId : Integer );
//
//  Notes:
//
//  1.- The bitmap is freed automatically inside the splash code, don't worry about it
//  2.- Use ShowSplashEx if you want:
//       a) Control whether the splash form is shown on top or not (default is true, using fsStayOnTop)
//       b) Use a non-rectangular form (Set TransparentColor to clNone if you want a normal rectangular
//          window, with any other color all matching pixels will be transparent in the image)
//  3.- You may want to display some information (eg. User Registration ID), just use the bitmap canvas
//      before calling ShowSplash

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    Buffer, SpeedBmp, SpeedImage, WinUtils, RegUtils;

  type
    TSplash =
      class( TForm )
         SplashImage : TSpeedImage;
        protected
          procedure EraseBkgnd( var Msg : TWMEraseBkgnd );                                          message WM_ERASEBKGND;
      end;

  type
    TFormCreationProc = procedure;

  procedure ShowSplash( Bitmap : TSpeedBitmap; AppInitialization : TFormCreationProc );
  procedure ShowSplashEx( Bitmap : TSpeedBitmap; AppInitialization : TFormCreationProc;
                          StayOnTop : boolean; UseRegistryFlag : boolean );

implementation

  // TSplash

  procedure TSplash.EraseBkgnd( var Msg : TWMEraseBkgnd );
    begin
    end;

  // ShowSplash

  procedure ShowSplashEx( Bitmap : TSpeedBitmap; AppInitialization : TFormCreationProc;
                          StayOnTop : boolean; UseRegistryFlag : boolean );
    var
      ShowSplash  : boolean;
      ShowFlagStr : string;
      ShowFlag    : integer;
    const
      ShowRegPath = 'Splash\ShowSplash';
    begin
      try
        with TSplash.Create( Application ) do
          try
            try
              ShowFlagStr := GetRegValue( HKEY_LOCAL_MACHINE, AppRegistryPath + ShowRegPath );
              ShowFlag    := StrToInt( ShowFlagStr );
            except
              ShowFlag := -1;
            end;
            if ShowFlag = -1 // Invalid or missing key, fix the value
              then
                try
                  SetRegValue( HKEY_LOCAL_MACHINE, AppRegistryPath + ShowRegPath, '1' );
                except
                end;

            ShowSplash := Assigned( Bitmap ) and ( ShowFlag <> 0 );
            if ShowSplash
              then
                begin
                  if StayOnTop
                    then FormStyle := fsStayOnTop
                    else FormStyle := fsNormal;

                  Caption := '';

                  ClientWidth         := Bitmap.Width;
                  ClientHeight        := Bitmap.Height;
                  SplashImage.Picture := Bitmap;

                  if Bitmap.Transparent
                    then SetWindowRgn( Handle, BitmapCreateRegion( Bitmap, BitmapColor( Bitmap, Bitmap.TransparentColor ) ), false );
                  Show;
                  Update;
                end;

            Application.Initialize;
            AppInitialization;

            if ShowSplash
              then Close;
          finally
            Free;
          end;
      finally
        Bitmap.Free;
      end;
    end;

  procedure ShowSplash( Bitmap : TSpeedBitmap; AppInitialization : TFormCreationProc );
    begin
      ShowSplashEx( Bitmap, AppInitialization, true, true );
    end;

{$R *.DFM}

end.
