unit ExpertInfo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, StdCtrls, ExptIntf, ToolIntf, ComCtrls, EditIntf, VirtIntf;

  type
    TExpert =
      class(TForm)
          RichEdit1: TRichEdit;
          MainMenu1: TMainMenu;
          Tools1: TMenuItem;
          ViewAllNameofMenu1: TMenuItem;
          StatusBar1: TStatusBar;
          ProjectInformation1: TMenuItem;
          Editor1: TMenuItem;
          ViewEditor1: TMenuItem;
    Resulse1: TMenuItem;
    ProjectResource1: TMenuItem;
          procedure ViewAllNameofMenu1Click(Sender: TObject);
          procedure ProjectInformation1Click(Sender: TObject);
          procedure ViewEditor1Click(Sender: TObject);
    procedure ProjectResource1Click(Sender: TObject);
        private
          procedure SetDebug( Value: string );
        public
          property Add: string write SetDebug;
      end;

    TMerchiseExpert =
      class( TIExpert )
        private
          MenuItem: TIMenuItemIntf;
        protected
          procedure OnClick( Sender: TIMenuItemIntf); virtual;
        public
          constructor Create;                         virtual;
          destructor Destroy;                         override;
          function GetName: string;                   override;
          function GetAuthor: string;                 override;
          function GetStyle: TExpertStyle;            override;
          function GetIDString: string;               override;

          function GetComment: string;                override;
          function GetPage: string;                   override;
          function GetGlyph: HICON;                   override;
          function GetState: TExpertState;            override;
          function GetMenuText: string;               override;
          procedure Execute; override;
      end;

  procedure Register;


implementation

{$R *.DFM}

  procedure Register;
    begin
      RegisterLibraryExpert( TMerchiseExpert.Create );
    end;

  constructor TMerchiseExpert.Create;
    var
      Main: TIMainMenuIntf;
      ReferenceMenuItem: TIMenuItemIntf;
      Menu: TIMenuItemIntf;
    begin
      inherited Create;
      MenuItem := nil;
      if Assigned( ToolServices )
        then
          begin
            Main := ToolServices.GetMainMenu;
            if Assigned( Main )
              then
                try
                  ReferenceMenuItem := Main.FindMenuItem('ToolsOptionsItem');
                  if Assigned( ReferenceMenuItem )
                    then
                      try
                        Menu := ReferenceMenuItem.GetParent;
                        if Assigned( Menu )
                          then
                            try
                              MenuItem := Menu.InsertItem(ReferenceMenuItem.GetIndex+1,
                                                '&Expert Info',
                                                'ExpertInfoMerchise','',
                                                0,0,0,
                                                [mfEnabled, mfVisible], OnClick);
                            finally
                              Menu.DestroyMenuItem;
                            end;
                        finally
                          ReferenceMenuItem.DestroyMenuItem;
                        end;
                finally
                  Main.Free;
                end;
          end;
    end;

  destructor TMerchiseExpert.Destroy;
    begin
      if Assigned( MenuItem )
        then MenuItem.DestroyMenuItem;
      inherited Destroy;
    end;

  function TMerchiseExpert.GetName: string;
    begin
      Result := 'Merchise'
    end;

  function TMerchiseExpert.GetAuthor: string;
    begin
      Result := 'Roberto Alonso Gómez';
    end;

  function TMerchiseExpert.GetStyle: TExpertStyle;
    begin
      Result := esAddIn;
    end;

  function TMerchiseExpert.GetIDString: String;
    begin
      Result := 'private.Merchise';
    end;

  function TMerchiseExpert.GetComment: string;
    begin
      result := '';
    end;

  function TMerchiseExpert.GetPage: string;
    begin
      result := '';
    end;

  function TMerchiseExpert.GetGlyph: HICON;
    begin
      result := 0;
    end;

  function TMerchiseExpert.GetState: TExpertState;
    begin
      result := [esEnabled];
    end;

  function TMerchiseExpert.GetMenuText: string;
    begin
      result := '';
    end;

  procedure TMerchiseExpert.Execute;
    begin
    end;

  procedure TMerchiseExpert.OnClick( Sender: TIMenuItemIntf);
    begin
      with TExpert.Create(Application) do
        try
          ShowModal;
        finally
          Free;
        end;
    end;

  // TExpert
  function GetFlagsMenu( Flags: TIMenuFlags ): string;
    begin
      if mfInvalid in Flags
        then result := '_mfInvalid'
        else result := '';
      if mfEnabled in Flags
        then result := '_mfEnabled';
      if mfVisible in Flags
        then result := '_mfVisible';
      if mfChecked in Flags
        then result := '_mfChecked';
      if mfBreak in Flags
        then result := '_mfBreak';
      if mfBarBreak in Flags
        then result := '_mfBarBreak';
      if mfRadioItem in Flags
        then result := '_mfRadioItem';
    end;

  procedure TExpert.SetDebug( Value: string );
    begin
      RichEdit1.Lines.add( Value );
    end;

  procedure TExpert.ViewAllNameofMenu1Click(Sender: TObject);
    const
      s = '                               ';
    var
      Main: TIMainMenuIntf;
      Menu: TIMenuItemIntf;
      procedure DoMenu( aMenu: TIMenuItemIntf; Ind: integer );
        var
          i     : integer;
          vMenu : TIMenuItemIntf;
          NameM : string;
        begin
          with aMenu do
            begin
              inc( Ind, 4 );
              NameM := GetName + '  [ ' + GetCaption + ' ]' + GetFlagsMenu( GetFlags );
              if NameM<>''
                then Add := copy( s, 1, Ind ) + NameM;
              for i := 0 to GetItemCount-1 do
                begin
                  vMenu := GetItem( i );
                  if assigned( vMenu )
                    then
                      try
                        DoMenu( vMenu, Ind );
                      finally
                        vMenu.release;
                      end;
                end;
            end;
        end;
    begin
      if Assigned( ToolServices )
        then
          begin
            Main := ToolServices.GetMainMenu;
            if Assigned( Main )
              then
                try
                  Menu := Main.GetMenuItems;
                  if assigned( Menu )
                    then
                      try
                        Add := ' Merchise Info Menu'#10'Name  [ Caption ] | Attributed'#10;
                        DoMenu( Menu, 0 );
                      finally
                        Menu.release;
                      end;
                finally
                  Main.release;
                end;
          end;
    end;

  procedure TExpert.ProjectInformation1Click(Sender: TObject);
    var
      i : integer;
    begin
      if Assigned( ToolServices )
        then
          with ToolServices do
            begin
              Add := ' ToolServices.GetProjectName     = ' + GetProjectName;
              Add := ' ToolServices.GetUnitCount       = ' + IntToStr( GetUnitCount );
              for i := 0 to GetUnitCount-1 do
                Add := ' GetUnitName( ' + IntToStr( i ) + ' ) = ' + GetUnitName( i );
              Add := ' ToolServices.GetCurrentFile     = ' + GetCurrentFile;
              Add := ' ToolServices.GetFormCount       = ' + IntToStr( GetFormCount );
              for i := 0 to GetFormCount-1 do
                Add := ' GetFormName( ' + IntToStr( i ) + ' ) = ' + GetFormName( i );

              Add := ' ToolServices.GetModuleCount     = ' + IntToStr( GetModuleCount );
              for i := 0 to GetModuleCount-1 do
                Add := ' GetModuleName( ' + IntToStr( i ) + ' ) = ' + GetModuleName( i ) +
                   '  GetComponentCount() = ' + IntToStr( GetComponentCount( i ));
              // Configuration Access
              Add := ' ToolServices.GetBaseRegistryKey = ' + GetBaseRegistryKey;
            end;
    end;

  procedure TExpert.ViewEditor1Click(Sender: TObject);
    var
      Module    : TIModuleInterface;
      Editor    : TIEditorInterface;
      View      : TIEditView;
      CharPos   : TCharPos;
    begin
      with ExptIntf.ToolServices do
        Module  := GetModuleInterface( GetCurrentFile );
      if Assigned( Module )
        then
          try
            Editor := Module.GetEditorInterface;
            if Assigned( Editor )
              then
                try
                  add := 'Editor.LinesInBufferm = ' + IntToStr( Editor.LinesInBuffer );
                  add := 'Editor.FileName       = ' + Editor.FileName;
                  add := 'Editor.GetViewCount   = ' + IntToStr( Editor.GetViewCount );
                  View := Editor.GetView( pred( Editor.GetViewCount ));
                  if Assigned( View )
                    then
                      try
                        add := 'View Ok';
                        with View.GetViewSize do
                          Add := 'ViewSize Cx = ' + IntToStr( cx ) + ' Cy = ' + IntToStr( cy );
                        with View.CursorPos do
                          Add := 'CursorPo Col = ' + IntToStr( Col ) + ' Line = ' + IntToStr( Line );
                        with View.TopPos do
                          Add := 'TopPos Col   = ' + IntToStr( Col ) + ' Line = ' + IntToStr( Line );

                        CharPos.CharIndex := 255;
                        CharPos.Line      := Editor.LinesInBuffer;
                        Add := ' Buffer Size 1 ' + IntToStr( View.CharPosToPos( CharPos ));
                        CharPos.CharIndex := 0;
                        CharPos.Line      := $7fffffff;
                        Add := ' Buffer Size 2 ' + IntToStr( View.CharPosToPos( CharPos ));
                      finally
                        View.free;
                      end;
                finally
                  Editor.free;
                end;
          finally
            Module.free;
          end;
    end;

  procedure TExpert.ProjectResource1Click(Sender: TObject);
    var
      Module    : TIModuleInterface;
      Resource  : TIResourceFile;
      Entry     : TIResourceEntry;
      i         : integer;
    begin
      with ExptIntf.ToolServices do
        Module  := GetModuleInterface( GetCurrentFile );
      if Assigned( Module )
        then
          try
            Resource := Module.GetProjectResource;
            if assigned( Resource )
              then
                with Resource do
                  try
                    Add := ' Resource.FileName      = ' + FileName;
                    Add := ' Resource.GetEntryCount = ' + IntToStr( GetEntryCount );
                    for i := 0 to GetEntryCount-1 do
                      begin
                        Entry := Resource.GetEntry( 1 );
                        if Assigned( Entry )
                          then
                            with Entry do
                              try
                                add := ' Ok ';
                                add := 'Entry.GetResourceName = ' + Entry.GetResourceName;
                          //      add := 'Entry.GetResourceType = ' + Entry.GetResourceType;
                              finally
                                free;
                              end;
                      end;
                  finally
                    free;
                  end
               else add := ' No tiene recurso ';
          finally
            Module.free;
          end;
    end;

end.


