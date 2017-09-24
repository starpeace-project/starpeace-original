unit TestForm;

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    SpeedBmp;

  type
    TForm1 =
      class(TForm)
          procedure FormCreate(Sender: TObject);
          procedure FormDblClick(Sender: TObject);
          procedure FormPaint(Sender: TObject);
        private
          { Private declarations }
        public
          { Public declarations }
          fBitmap    : TSpeedBitmap;
          fTextDrawn : boolean;
      end;

  var
    Form1: TForm1;

implementation

  {$R *.DFM}

  procedure TForm1.FormCreate(Sender: TObject);
    begin
      fBitmap := TSpeedBitmap.CreateSized(ClientWidth, ClientHeight, 16);
    end;

  procedure TForm1.FormDblClick(Sender: TObject);
    const
      msg : string = 'This is a fucking test';
    begin
      with fBitmap.Canvas do
        begin
          Font.Color := clRed;
          Windows.TextOut(Handle, 0, 0, pchar(msg), length(msg));
        end;
      fTextDrawn := true;
      Invalidate;
    end;

  procedure TForm1.FormPaint(Sender: TObject);
    begin
      if fTextDrawn
        then fBitmap.ClipDraw(Canvas, 0, 0, ClientRect);
    end;

end.

