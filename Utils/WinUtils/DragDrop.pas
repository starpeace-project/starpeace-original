unit DragDrop;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise.

interface

  uses
    Classes, Windows, Messages, ShellAPI;

  // Drag&Drop Target -----------------------------------------------------------------

  type
    TFileDroppedEvent     = procedure( Sender : TObject; const DropPoint : TPoint; const FileName : string   ) of object;
    TFileListDroppedEvent = procedure( Sender : TObject; const DropPoint : TPoint; const Files : TStringList ) of object;

  type
    TDropTarget =
      class
        protected
          fHandle            : HWND;
          fOnFileListDropped : TFileListDroppedEvent;
          fOnFileDropped     : TFileDroppedEvent;
          fOldAcceptFiles    : boolean;
          fWndProcData       : integer;
          fAcceptOnlyOneFile : boolean;

        public
          property Handle : HWND read fHandle;

        public
          property OnFileDropped     : TFileDroppedEvent     read fOnFileDropped     write fOnFileDropped;
          property OnFileListDropped : TFileListDroppedEvent read fOnFileListDropped write fOnFileListDropped;
          property AcceptOnlyOneFile : boolean               read fAcceptOnlyOneFile write fAcceptOnlyOneFile;

        public
          constructor Create( WinHandle : HWND );
          destructor  Destroy;                                                       override;

        protected
          procedure HandleDropFilesMessage( DropHandle : HDROP );
      end;

  // Drag&Drop Source -----------------------------------------------------------------

  function DropFile( WinHandle : HWND; const FileName : string ) : boolean;

  // Low level routines

  function DropCreate : HDROP;
  function DropAddFile( var Handle : HDROP; FileName : pchar ) : boolean;

  function DragCreateFiles( const aMousePos : TPoint; aInNonClientArea : BOOL; aUnicode : BOOL ): HDROP;
  function DragAppendFile( DropHandle : HDROP; PathName : pointer ): HDROP; cdecl;

implementation

  uses
    WinUtils;

  var
    DropTargets : TList = nil;

  function DragDropWndProc( WinHandle : HWND; Msg : integer; wPar : WPARAM; lPar : LPARAM ) : LRESULT; stdcall;
    var
      i : integer;
    begin
      try
        i := 0;
        while (i < DropTargets.Count) and (TDropTarget(DropTargets[i]).Handle <> WinHandle) do
          inc( i );
        with TDropTarget(DropTargets[i]) do  // This will raise an exception if indx not found
          if Msg = WM_DROPFILES
            then
              begin
                HandleDropFilesMessage( wPar );
                Result := 0;
              end
            else Result := CallWindowProc( PrevWndProc( fWndProcData ), WinHandle, Msg, wPar, lPar );
      except
        Result := 0;
      end;
    end;

  constructor TDropTarget.Create( WinHandle : HWND );
    begin
      inherited Create;

      fHandle      := WinHandle;
      fWndProcData := ChangeWndProc( fHandle, @DragDropWndProc );

      if not Assigned( DropTargets )
        then DropTargets := TList.Create;
      DropTargets.Add( Self );

      fOldAcceptFiles := WindowIsDropTarget( fHandle );
      if not fOldAcceptFiles
        then DragAcceptFiles( fHandle, true );
    end;

  destructor TDropTarget.Destroy;
    begin
      if Assigned( DropTargets )
        then
          begin
            if not fOldAcceptFiles
              then DragAcceptFiles( fHandle, false );

            RestoreWndProc( fHandle, fWndProcData );

            DropTargets.Remove( Self );
            DropTargets.Pack;
            if DropTargets.Count = 0
              then
                begin
                  DropTargets.Free;
                  DropTargets := nil;
                end;
          end;

      inherited;
    end;

  procedure TDropTarget.HandleDropFilesMessage( DropHandle : HDROP );
    var
      i            : integer;
      DropPoint    : TPoint;
      DropCount    : integer;
      DroppedFiles : TStringList;
      Filename     : string;
      FilenameSize : integer;
    begin
      DroppedFiles := TStringList.Create;
      DragQueryPoint( DropHandle, DropPoint );

      if AcceptOnlyOneFile
        then DropCount := 1
        else DropCount := DragQueryFile( DropHandle, -1, nil, 0 );
      for i := 0 to DropCount - 1 do
        begin
          FileNameSize := DragQueryFile( DropHandle, i, nil, 0 );
          SetLength( Filename, FilenameSize );
          DragQueryFile( DropHandle, i, pchar(Filename), FilenameSize + 1 );
          DroppedFiles.Add( Filename );
        end;
      if Assigned( OnFileListDropped )
        then OnFileListDropped( Self, DropPoint, DroppedFiles );
      if Assigned( OnFileDropped )
        then
          for i := 0 to DropCount - 1 do
            OnFileDropped( Self, DropPoint, DroppedFiles[i] );

      DragFinish( DropHandle );
      DroppedFiles.Free;
    end;

  // Drag&Drop Source -----------------------------------------------------------------

  type
    PDropFileStruct = ^TDropFileStruct;
    TDropFileStruct =
      packed record
        Size            : word;
        MousePos        : TPoint;
        InNonClientArea : BOOL;
        Unicode         : BOOL;
      end;

  function DropFile( WinHandle : HWND; const FileName : string ) : boolean;
    var
      DropHandle : HDROP;
    begin
      try
        DropHandle := DropCreate;
        if DropAddFile( DropHandle, pchar(FileName) )
          then
            begin
              Result := true;
              PostMessage( WinHandle, WM_DROPFILES, DropHandle, 0 );
            end
          else Result := false;
      except
        Result := false;
      end;
    end;

  // Low level routines

  function DropCreate : HDROP;
    begin
      Result := DragCreateFiles( Point( 0, 0 ), false, false );
    end;

  function DropAddFile( var Handle : HDROP; FileName : pchar ) : boolean;
    var
      DropHandle : HDROP;
    begin
      DropHandle := DragAppendFile( Handle, FileName );
      if DropHandle <> 0
        then
          begin
            Result := true;
            Handle := DropHandle;
          end
        else Result := false;
    end;

  //

  function DragCreateFiles( const aMousePos : TPoint; aInNonClientArea : BOOL; aUnicode : BOOL ): HDROP;
    var
      DropFileStruct : PDropFileStruct;
    begin
	// Allocate dynamic memory for the DROPFILESTRUCT data
	// structure and for the extra zero-character identifying
	// that there are no pathnames in the block yet.
	if aUnicode
          then Result := GlobalAlloc( GMEM_MOVEABLE or GMEM_ZEROINIT, sizeof(TDropFileStruct) + sizeof(WideChar) )
          else Result := GlobalAlloc( GMEM_MOVEABLE or GMEM_ZEROINIT, sizeof(TDropFileStruct) + sizeof(AnsiChar) );
        if Result <> 0
          then
            begin
              // Lock block and initialize the data members
              DropFileStruct := GlobalLock( Result );
              with DropFileStruct^ do
                begin
                  Size            := sizeof( TDropFileStruct );
                  MousePos        := aMousePos;
                  InNonClientArea := aInNonClientArea;
                  Unicode         := aUnicode;
                end;
              GlobalUnlock( Result );
            end;
    end;

  function DragAppendFile( DropHandle : HDROP; PathName : pointer ): HDROP;
    var
      DropFileStruct      : PDropFileStruct;
      PathNameAnsi        : PAnsiChar absolute PathName;
      PathNameUnicode     : PWideChar absolute PathName;
      PathAnsi            : PAnsiChar;
      PathUnicode         : PWideChar;
      OffsetOfNewPathname : integer;
      PathSize            : integer;
    begin
      DropFileStruct := GlobalLock( DropHandle );

      // Point to first pathname in list
      PathAnsi    := PAnsiChar(DropFileStruct) + sizeof(TDropFileStruct);
      PathUnicode := pointer(PathAnsi);

      if DropFileStruct.Unicode
        then
          begin
            // Search for a pathname where 1st char is a zero-char
            while PathUnicode[0] <> #0 do     // While the 1st char is non-zero
              begin
                while PathUnicode[0] <> #0 do // Find end of current path
                  inc( PathUnicode );
                inc( PathUnicode );           // Skip over the zero-char
              end;

            // Get the offset from the beginning of the block
            // where the new pathname should go
            OffsetOfNewPathname := PAnsiChar(PathUnicode) - PAnsiChar(DropFileStruct);

            // Get the number of bytes needed for the new pathname,
            // it's terminating zero-char, and the zero-length
            // pathname that marks the end of the list of pathnames
            PathSize := sizeof(WideChar) * (lstrlenW(PathnameUnicode) + 2);
          end
        else
          begin
            // Search for a pathname where 1st char is a zero-char
            while PathAnsi[0] <> #0 do     // While the 1st char is non-zero
              begin
                while PathAnsi[0] <> #0 do // Find end of current path
                  inc( PathAnsi );
                inc( PathAnsi );           // Skip over the zero-char
              end;

            // Get the offset from the beginning of the block
            // where the new pathname should go
            OffsetOfNewPathname := PAnsiChar(PathAnsi) - PAnsiChar(DropFileStruct);

            // Get the number of bytes needed for the new pathname,
            // it's terminating zero-char, and the zero-length
            // pathname that marks the end of the list of pathnames
            PathSize := sizeof(AnsiChar) * (lstrlenA(PathnameAnsi) + 2);
          end;

      GlobalUnlock( DropHandle );
      // Increase block size to accommodate new pathname
      Result := GlobalRealloc( DropHandle, PathSize + OffsetOfNewPathname, GMEM_MOVEABLE or GMEM_ZEROINIT );

      if Result <> 0
        then
          begin
            DropFileStruct := GlobalLock( Result );
            with DropFileStruct^ do
              begin
                // Append the pathname to the end of the block
                if Unicode
                  then Move( PathnameUnicode[0], PAnsiChar(DropFileStruct)[OffsetOfNewPathname], lstrlenW(PathnameUnicode) * sizeof(WideChar) )
                  else Move( PathnameAnsi[0], PAnsiChar(DropFileStruct)[OffsetOfNewPathname], lstrlenA(PathnameAnsi) * sizeof(AnsiChar) );
            GlobalUnlock( Result );
          end;
        end;
    end;

end.

