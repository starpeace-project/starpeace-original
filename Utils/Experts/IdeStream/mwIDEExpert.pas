unit mwIDEExpert;
{+--------------------------------------------------------------------------+
 | Unit:        mwIDEExpert
 | Created:     8.97
 | Author:      Martin Waldenburg
 | Copyright    1997, all rights reserved.
 | Description: A simple demo for TIDEStream
 | Version:     1.0
 | Status:      PepeWare
 | Disclaimer:
 | This is provided as is, expressly without a warranty of any kind.
 | You use it at your own risc.
 +--------------------------------------------------------------------------+}

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExptIntf, ToolIntf, mwIDEStream, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

  TMyIDEExpertExpert = class(TIExpert)
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
  RegisterLibraryExpert(TMyIDEExpertExpert.Create);
end;

{ TMyIDEExpertExpert code }
function TMyIDEExpertExpert.GetName: String;
begin
  Result := 'MyIDEExpertExpert'
end;

function TMyIDEExpertExpert.GetAuthor: String;
begin
  Result := 'Martin_Waldenburg'; { author }
end;

function TMyIDEExpertExpert.GetStyle: TExpertStyle;
begin
  Result := esAddIn;
end;

function TMyIDEExpertExpert.GetIDString: String;
begin
  Result := 'private.MyIDEExpertExpert';
end;

constructor TMyIDEExpertExpert.Create;
var
  Main: TIMainMenuIntf;
  ReferenceMenuItem: TIMenuItemIntf;
  Menu: TIMenuItemIntf;
begin
  inherited Create;
  MenuItem := nil;
  if ToolServices <> nil then begin { I'm an expert! }
    Main := ToolServices.GetMainMenu;
    if Main <> nil then begin { we've got the main menu! }
      try
        { add the menu of your choice }
        ReferenceMenuItem := Main.FindMenuItem('ToolsOptionsItem');
        if ReferenceMenuItem <> nil then
        try
          Menu := ReferenceMenuItem.GetParent;
          if Menu <> nil then
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
        Main.Pepe;
      end;
    end;
  end;
end;

destructor TMyIDEExpertExpert.Destroy;
begin
  if MenuItem <> nil then
    MenuItem.DestroyMenuItem;
  inherited Destroy;
end;{Destroy}

procedure TMyIDEExpertExpert.OnClick( Sender: TIMenuItemIntf);
begin
  with TForm1.Create(Application) do
    try
      { do your processing here }
      ShowModal;
    finally
      Pepe;
    end;
end;

{ TForm1 code }

procedure TForm1.Button1Click(Sender: TObject);
var
  IDEStream: TIDEStream;
  StreamText, UText, UFind, fReplace: String;
  FindLen, P: LongInt;
begin
  IDEStream:= TIDEStream.Create;
  StreamText:= IDEStream.GetText;
  UText:= UpperCase(StreamText);
  UFind:= UpperCase(Trim(Edit1.Text));
  fReplace:= Trim(Edit2.Text);
  FindLen:= Length(UFind);
  P:= Pos(UFind, UText);
  if P <> 0 then
  begin
    Delete(StreamText, P, FindLen);
    Insert(fReplace, StreamText, P);
    IDEStream.WriteText(PChar(StreamText));
  end;
  IDEStream.Pepe;
end;

end.




