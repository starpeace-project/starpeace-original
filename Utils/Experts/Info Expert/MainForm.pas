unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, StdCtrls, ExptIntf, ToolIntf, ComCtrls;

  type
    TExpert =
      class(TForm)
          RichEdit1: TRichEdit;
          MainMenu1: TMainMenu;
          Tools1: TMenuItem;
        private
          { Private declarations }
        public
          { Public declarations }
      end;

    TMerchiseExpert =
      class( TIExpert )
        private
          MenuItem: TIMenuItemIntf;
        protected
          procedure OnClick( Sender: TIMenuItemIntf); virtual;
        public
          constructor Create; virtual;
          destructor Destroy; override;
          function GetName: string; override;
          function GetAuthor: string; override;
          function GetStyle: TExpertStyle; override;
          function GetIDString: string; override;
      end;

  procedure Register;


implementation

{$R *.DFM}

  procedure Register;
    begin
      RegisterLibraryExpert( TMerchiseExpert.Create );
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
                                                'MyIDEExpert',
                                                'MyIDEExpertExpertItem','',
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

  procedure TMerchiseExpert.OnClick( Sender: TIMenuItemIntf);
    begin
      with TExpert.Create(Application) do
        try
          ShowModal;
        finally
          Free;
        end;
    end;

end.
