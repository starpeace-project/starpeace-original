unit ConvToIsometricTestWindow;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls;

type
  TForm1 = class(TForm)
    Original: TImage;
    Converted: TImage;
    Convert: TBitBtn;
    Load: TBitBtn;
    OpenDialog: TOpenDialog;
    SaveIt: TBitBtn;
    SaveDialog: TSaveDialog;
    procedure LoadClick(Sender: TObject);
    procedure ConvertClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SaveItClick(Sender: TObject);
  private
    { Private declarations }
    fCurrentBitmap : TBitmap;
    fTemplate      : TBitmap;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

uses
  ConvertToIsometric;

procedure TForm1.LoadClick(Sender: TObject);
  begin
    if OpenDialog.Execute
      then
        begin
          fCurrentBitmap.Free;
          fCurrentBitmap := TBitmap.Create;
          fCurrentBitmap.LoadFromFile(OpenDialog.FileName);
          Original.Picture.Bitmap := fCurrentBitmap;
        end;
  end;

procedure TForm1.ConvertClick(Sender: TObject);
  begin
    if fCurrentBitmap <> nil
      then Converted.Picture.Bitmap := ConvertBmpToIsometric(fTemplate, fCurrentBitmap);
  end;

procedure TForm1.FormCreate(Sender: TObject);
  begin
    fTemplate := TBitmap.Create;
    fTemplate.LoadFromFile('template.bmp');
  end;

procedure TForm1.FormDestroy(Sender: TObject);
  begin
    fTemplate.Free;
  end;

procedure TForm1.SaveItClick(Sender: TObject);
  begin
    SaveDialog.FileName := OpenDialog.FileName;
    if SaveDialog.Execute
      then Converted.Picture.Bitmap.SaveToFile(SaveDialog.FileName);
  end;

end.
