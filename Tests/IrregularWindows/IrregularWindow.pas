unit IrregularWindow;

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

  type
    TIrregularForm =
      class(TForm)
          procedure FormDblClick(Sender: TObject);
        private
          { Private declarations }
        public
          { Public declarations }
          fRound : boolean;
      end;

  var
    IrregularForm: TIrregularForm;

implementation

  {$R *.DFM}

  procedure TIrregularForm.FormDblClick(Sender: TObject);
    var
      rgn  : HRGN;
      rgn1 : HRGN;
    begin
      if fRound
        then
          begin
            rgn := CreateRectRgn(0, 0, Width, Height);
            fRound := false;
          end
        else
          begin
            //rgn := CreateEllipticRgn(0, 0, Width, Height);
            rgn := CreateRectRgn(0, 0, Width, Height div 4);
            rgn1 := CreateRectRgn(0, Height - Height div 4, Width, Height);
            CombineRgn(rgn, rgn, rgn1, RGN_OR);
            fRound := true;
          end;
      SetWindowRgn(Handle, rgn, true);
    end;

end.
