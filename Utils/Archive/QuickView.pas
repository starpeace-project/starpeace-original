unit QuickView;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise.

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ExtCtrls,
    Win95FileViewers, WinEnum, DragDrop;

  type
    TQuickView =
      class( TCustomControl )
        published
          property Align;
          property Enabled;
          property ParentShowHint;
          property ShowHint;
          property Visible;

        protected
          fFileName : string;

          procedure SetFileName( aFileName : string );

        published
          property FileName : string read fFileName write SetFileName;

        private
          fFileViewer : IFileViewer;
          fTimeOut    : integer;

          procedure SetTimeOut( aTimeOut : integer );

        public
          property FileViewer : IFileViewer read fFileViewer;
          property TimeOut    : integer     read fTimeOut  write SetTimeOut;

        public
          constructor Create( aOwner : TComponent );                          override;
          destructor  Destroy;                                               override;

        private
          ViewerSite : IFileViewerSite;
          WinHandle  : HWND;
          WinClass   : string;
          WinMenu    : HMENU;
          ViewHandle : HWND;
          ViewRect   : TRect;
          ViewParent : HWND;
          ShowInfo   : TFileViewerShowInfo;

          procedure ReleaseFileViewer;

          function  QuickViewWindow : HWND;
          procedure PatchQuickViewWindow;

          procedure WMSize( var Message : TWMSize );                          message WM_SIZE;
      end;

  // Component registration

  procedure Register;

implementation

  function FillShowInfo( Control : TWinControl;
                    var ShowInfo : TFileViewerShowInfo ) : PFileViewerShowInfo;
    begin
      with ShowInfo do
        begin
          Size  := sizeof(ShowInfo );
          Owner := Control.Handle;
          Show  := SW_HIDE;
          Flags := FVSIF_CANVIEWIT or FVSIF_PINNED;
          with Control do
            begin
              ShowInfo.Rect.TopLeft     := ClientToScreen( Point( 0, 0 ) );
              ShowInfo.Rect.BottomRight := ClientToScreen( Point( Width, Height ) );
            end;
          UnkRel := nil;
        end;
      Result := @ShowInfo;
    end;

  procedure ShowViewer( var FileViewer : IFileViewer; ShowInfo : PFileViewerShowInfo );
    begin
      if FileViewer <> nil
        then
          begin
            FileViewer.Show( ShowInfo );
            FileViewer.Release;
            FileViewer := nil;
          end;
    end;

  // TQuickView

  procedure TQuickView.WMSize( var Message : TWMSize );
    begin
      inherited;
      if not (csLoading in ComponentState )
        then
          with Message do
            SetWindowPos( ViewHandle, HWND_TOP, 0, 0, Width, Height, SWP_NOACTIVATE );
    end;

  constructor TQuickView.Create( aOwner : TComponent );
    begin
      inherited;
      Width       := 60;
      Height      := 60;
      Caption     := '';
      TimeOut     := 60 * 1000;
    end;

  destructor TQuickView.Destroy;
    begin
      ReleaseFileViewer;
      inherited;
    end;

  procedure TQuickView.ReleaseFileViewer;
    var
      ThreadId : integer;
    begin
      if fFileViewer <> nil
        then
          begin
            Windows.SetParent( ViewHandle, ViewParent );
            with ViewRect do
              SetWindowPos( ViewHandle, HWND_TOP, Left, Top, Right, Bottom, SWP_NOREDRAW );

            SetMenu( WinHandle, WinMenu );
            ThreadId := GetWindowThreadProcessId( WinHandle, nil );
            if WinClass <> 'FileViewer'
              then PostThreadMessage( ThreadId, WM_QUIT, 0, 0 );

            fFileViewer.Release;
            fFileViewer := nil;
          end;
    end;

  procedure TQuickView.SetTimeOut( aTimeOut : integer );
    const
      MinTimeOut = 30 * 1000;
    begin
      if aTimeOut > MinTimeOut
        then fTimeOut := aTimeOut
        else fTimeOut := MinTimeOut;
    end;

  function TQuickView.QuickViewWindow : HWND;
    var
      StartTime  : integer;
    begin
      //Result := ViewerSite.WinHandle;
      Result := FindWindowByCaption( FileName, ['FileViewer'], WinClass );
      if Result = 0
        then
          begin
            ShowViewer( fFileViewer, FillShowInfo( Self, ShowInfo ) );

            StartTime := GetTickCount;
            repeat
              Result := FindWindowByCaption( FileName, ['OIWin95Frame'], WinClass );
            until (Result <> 0 ) or (GetTickCount - StartTime > TimeOut );
          end;
    end;

  procedure TQuickView.PatchQuickViewWindow;
    const
      StyleHidden = WS_CLIPSIBLINGS or WS_GROUP;
      StyleShown  = StyleHidden or WS_VISIBLE or WS_CHILDWINDOW;
      ExStyle     = WS_EX_LEFT or WS_EX_LTRREADING or WS_EX_RIGHTSCROLLBAR;
    begin
      SetWindowLong( WinHandle, GWL_STYLE, StyleHidden );
      SetWindowLong( WinHandle, GWL_EXSTYLE, ExStyle );

      WinMenu := GetMenu( WinHandle );
      SetMenu( WinHandle, 0 );

      ViewHandle := FindViewerClient( WinHandle );

      GetWindowRect( ViewHandle, ViewRect );

      ViewParent := Windows.GetParent( ViewHandle );
      Windows.SetParent( ViewHandle, Handle );

      SetWindowLong( ViewHandle, GWL_STYLE, StyleShown );
      SetWindowPos( ViewHandle, HWND_TOP, 0, 0, ClientWidth, ClientHeight, SWP_NOACTIVATE );
      Windows.ShowWindow( WinHandle, SW_HIDE );
    end;

  procedure TQuickView.SetFileName( aFileName : string );
    begin
      fFileName := ExpandFileName( aFileName );
      if fFileName = ''
        then ReleaseFileViewer
        else
          if ( FileViewer = nil ) or ( WinHandle = 0 )
            or ( not DropFile( WinHandle, fFileName ) ) // Fake a drag&drop on QuickView Window
            then // Create file viewer
              begin
                ReleaseFileViewer;

                //if ViewerSite = nil
                //  then ViewerSite := IFileViewerSite.Create(TForm(Owner ).Handle );
                fFileViewer := GetFileViewer( FileName, ViewerSite );

                if fFileViewer <> nil
                  then
                    begin
                      WinHandle := QuickViewWindow;
                      if WinHandle <> 0
                        then PatchQuickViewWindow
                        else Caption := 'Could not attach to QuickView window';
                    end
                  else Caption := 'Could not find a viewer for ''' + FileName + '''';
              end;
    end;

  // Registration

  procedure Register;
    begin
      RegisterComponents('Merchise', [TQuickView] );
    end;

end.
