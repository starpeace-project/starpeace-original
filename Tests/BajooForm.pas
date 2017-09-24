unit BajooForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, FileUpdater;

type
  TForm2 = class(TForm)
    Button2: TButton;
    Button3: TButton;
    ReqEdit: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  public
    procedure OnThreadTerminate(Sender: TObject);
  end;

var
  Form2: TForm2;

implementation

  {$R *.DFM}

  uses
    FileImage, RequestManager;

  procedure TForm2.OnThreadTerminate(Sender: TObject);
    begin
      if TFileUpdater(Sender).Index = 10
        then
          begin
            ShowMessage(IntToStr(WriteFails) + ' ' + IntToStr(ReadFails));
            Close;
          end;
    end;

  procedure TForm2.Button1Click(Sender: TObject);
    var
      i : integer;
    begin
      for i := 1 to 10 do
        TFileUpdater.Create('e:\tmp\bajoo.txt');
    end;

  procedure TForm2.Button2Click(Sender: TObject);
    var
      FI : TFileImage;
    begin
      FI := TFileImage.Create('e:\tmp\bajoo.txt');
      FI.Properties['Value'] := 'Kaka';
      ShowMessage(FI.Properties['Value']);
      //FI.Flush;
      FI.Free;
    end;

  procedure TForm2.Button3Click(Sender: TObject);
    begin
      ProcessRequest(ReqEdit.Text);
    end;

end.
