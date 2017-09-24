unit TestProxyFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CacheObjects, StdCtrls, ExtCtrls;

type
  TTestCacheProxy = class(TForm)
    Path: TEdit;
    Build: TButton;
    Link: TButton;
    Unlink: TButton;
    SetPath: TButton;
    Label1: TLabel;
    Button1: TButton;
    procedure FlushClick(Sender: TObject);
    procedure LinkClick(Sender: TObject);
    procedure UnlinkClick(Sender: TObject);
    procedure SetPathClick(Sender: TObject);
    procedure DeleteClick(Sender: TObject);
    procedure BuildClick(Sender: TObject);
  private
    fProxy : OleVariant;
    fCache : TCachedObject;
  end;

var
  TestCacheProxy: TTestCacheProxy;

implementation

{$R *.DFM}

  procedure TTestCacheProxy.FlushClick(Sender: TObject);
    begin
      fProxy.Flush;
    end;

  procedure TTestCacheProxy.LinkClick(Sender: TObject);
    begin
      fCache.CreateLink(Path.Text);
    end;

  procedure TTestCacheProxy.UnlinkClick(Sender: TObject);
    begin
      fCache.DeleteLink(Path.Text);
    end;

  procedure TTestCacheProxy.SetPathClick(Sender: TObject);
    begin
      fCache.Path := Path.Text;
      //Value.Text  := fCache['Name'];
    end;

  procedure TTestCacheProxy.DeleteClick(Sender: TObject);
    begin
      fCache.Delete;
    end;

  procedure TTestCacheProxy.BuildClick(Sender: TObject);
    begin
      fCache := TCachedObject.Create(Path.Text, nil);
      fCache['Name']  := 'aaaaaaaaaaaaaaaaa';
      fCache['Value'] := '12';
      fCache.Flush;
    end;


initialization

  SetCacheBasePath('e:\test\');

end.
