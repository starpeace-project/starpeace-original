unit LookForFolder;

interface

  uses
    Windows, SysUtils, Forms;

  function BrowseDirectory(ActiveForm : TForm; const BrowseTitle : string) : string;

implementation

  const
    BIF_RETURNONLYFSDIRS = $0001;

  type
    TBFFCallBack = function(hwnd : THandle; Msg : UINT; lParam : LPARAM; lpData : LPARAM) : integer;

  type
    PShellItemID = ^TShellItemID;
    TShellItemID =
      record
        Count : word;
        Data  : array[0..1] of byte;
      end;

  type
    PItemIDList = ^TItemIDList;
    TItemIDList =
      record
        ID : TShellItemID;
      end;

  type
    PBrowseInfo = ^TBrowseInfo;
    TBrowseInfo =
      record
        Owner       : THandle;
        Root        : PItemIDList;
        DisplayName : pchar;         // Return display name of item selected.
        Title       : pchar;         // text to go in the banner over the tree.
        Flags       : UINT;          // Flags that control the return stuff
        funct       : TBFFCallBack;
        Param       : LPARAM;        // extra info that's passed back in callbacks

        Image       : integer;       // output var: where to return the Image index.
      end;

  function SHBrowseForFolder(lpbi : PBrowseInfo) : PItemIDList;          stdcall; external 'Shell32.dll';
  function SHGetPathFromIDList(pidl : PItemIDList; Path : pchar) : BOOL; stdcall; external 'Shell32.dll'

  function BrowseDlg(ActiveForm : TForm; const BrowseTitle : string; RootFolder : PItemIDList; Options : integer) : string;
    var
      pidl       : PItemIDList;
      BrowseInfo : TBrowseInfo;
      SelName    : array [0..MAX_PATH] of char;
    begin
      with BrowseInfo do
        begin
          if ActiveForm <> nil
            then Owner := ActiveForm.Handle
            else Owner := Application.Handle;
          Root        := RootFolder;
          DisplayName := SelName;
          Title       := PCHAR(BrowseTitle);
          Flags       := Options;
          funct       := nil;
          Param       := 0;
        end;
      pidl := SHBrowseForFolder(@BrowseInfo);
      if pidl <> nil
        then
          begin
            SetLength(result, MAX_PATH);
            if SHGetPathFromIDList(pidl, PCHAR(result))
              then SetLength(result, strlen(PCHAR(result)))
              else SetLength(result, 0);
          end
        else result := '';
    end;

  function BrowseDirectory(ActiveForm : TForm; const BrowseTitle : string) : string;
    begin
      Result := BrowseDlg(ActiveForm, BrowseTitle, nil, BIF_RETURNONLYFSDIRS);
    end;

end.

