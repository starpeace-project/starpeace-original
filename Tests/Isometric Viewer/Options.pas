unit Options;

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    StdCtrls;

  type
    TOptionsForm =
      class(TForm)
          AnimateBuildings: TCheckBox;
          SoundsEnabled: TCheckBox;
          AnimateLand: TCheckBox;
          GlassBuildings: TCheckBox;
          CarsEnabled: TCheckBox;
          PlanesEnabled: TCheckBox;
          Button1: TButton;
          Button2: TButton;
        private
          { Private declarations }
        public
          { Public declarations }
      end;

  var
    OptionsForm: TOptionsForm;

implementation

  {$R *.DFM}









end.
