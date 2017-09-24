unit ShellGUID;

interface

  const
    CLSID_ShellDesktop : TGUID =
      (
        D1 : $00021400;
        D2 : 0;
        D3 : 0;
        D4 : ( $C0, 0, 0, 0, 0, 0, 0, $46 );
      );

    CLSID_ShellLink : TGUID =
      (
        D1 : $00021401;
        D2 : 0;
        D3 : 0;
        D4 : ( $C0, 0, 0, 0, 0, 0, 0, $46 );
      );

    IID_IContextMenu : TGUID =
      (
        D1 : $000214E4;
        D2 : 0;
        D3 : 0;
        D4 : ( $C0, 0, 0, 0, 0, 0, 0, $46 );
      );

    IID_IShellFolder : TGUID =
      (
        D1 : $000214E6;
        D2 : 0;
        D3 : 0;
        D4 : ( $C0, 0, 0, 0, 0, 0, 0, $46 );
      );

    IID_IShellExtInit : TGUID =
      (
        D1 : $000214E8;
        D2 : 0;
        D3 : 0;
        D4 : ( $C0, 0, 0, 0, 0, 0, 0, $46 );
      );

    IID_IShellPropSheetExt : TGUID =
      (
        D1 : $000214E9;
        D2 : 0;
        D3 : 0;
        D4 : ( $C0, 0, 0, 0, 0, 0, 0, $46 );
      );

    IID_IExtractIcon : TGUID =
      (
        D1 : $000214EB;
        D2 : 0;
        D3 : 0;
        D4 : ( $C0, 0, 0, 0, 0, 0, 0, $46 );
      );

    IID_IShellLink : TGUID =
      (
        D1 : $000214EE;
        D2 : 0;
        D3 : 0;
        D4 : ( $C0, 0, 0, 0, 0, 0, 0, $46 );
      );

    IID_IShellCopyHook : TGUID =
      (
        D1 : $000214EF;
        D2 : 0;
        D3 : 0;
        D4 : ( $C0, 0, 0, 0, 0, 0, 0, $46 );
      );

    IID_IFileViewer : TGUID =
      (
        D1 : $000214F0;
        D2 : 0;
        D3 : 0;
        D4 : ( $C0, 0, 0, 0, 0, 0, 0, $46 );
      );

    IID_IEnumIDList : TGUID =
      (
        D1 : $000214F2;
        D2 : 0;
        D3 : 0;
        D4 : ( $C0, 0, 0, 0, 0, 0, 0, $46 );
      );

    IID_IFileViewerSite : TGUID =
      (
        D1 : $000214F3;
        D2 : 0;
        D3 : 0;
        D4 : ( $C0, 0, 0, 0, 0, 0, 0, $46 );
      );

    IID_IShellView : TGUID =
      (
        D1 : $000214E3;
        D2 : 0;
        D3 : 0;
        D4 : ( $C0, 0, 0, 0, 0, 0, 0, $46 );
      );

    IID_IShellBrowser : TGUID =
      (
        D1 : $000214E2;
        D2 : 0;
        D3 : 0;
        D4 : ( $C0, 0, 0, 0, 0, 0, 0, $46 );
      );

implementation

end.
