unit TestClientFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TClientForm = class(TForm)
    Cache: TButton;
    Path: TEdit;
    Label1: TLabel;
    Links: TEdit;
    Label2: TLabel;
    Address: TEdit;
    Label3: TLabel;
    procedure CacheClick(Sender: TObject);
    procedure AddressChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ClientForm: TClientForm;

implementation

uses
  CacheCommon, MSObjectCacher;

{$R *.DFM}

  type
    TFormCacher =
      class(TCacheAgent)
        class function GetPath (Obj : TObject) : string;           override;
        class function GetCache(Obj : TObject) : TObjectCache;     override;
        class function UpdateCache(Obj : TObject) : TObjectCache;  override;
      end;

  class function TFormCacher.GetPath(Obj : TObject) : string;
    begin
      with TClientForm(Obj) do
        result := Path.Text;
    end;

  class function TFormCacher.GetCache(Obj : TObject) : TObjectCache;
    begin
      result := TObjectCache.Create;
      with TClientForm(Obj) do
        begin
          result.WriteString('Caption', Caption);
          result.WriteInteger('Top', Top);
          result.WriteInteger('Width', Width);
          result.Links := Links.Text;
        end;
    end;

  class function TFormCacher.UpdateCache(Obj : TObject) : TObjectCache;
    begin
      result := TObjectCache.Create;
      with TClientForm(Obj) do
        begin
          result.WriteString('Caption', Caption);
          result.WriteInteger('Top', Top);
          result.WriteInteger('Width', Width);
        end
    end;


  procedure TClientForm.CacheClick(Sender: TObject);
    begin
      CacheObject(Self);
    end;

  procedure TClientForm.AddressChange(Sender: TObject);
    begin
      WSURL := Address.Text;
    end;

initialization

  RegisterCacher('TClientForm', TFormCacher);

end.
