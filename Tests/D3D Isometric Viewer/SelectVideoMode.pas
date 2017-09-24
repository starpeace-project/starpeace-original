unit SelectVideoMode;

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    StdCtrls;

  type
    TSelectVideoModeForm =
      class(TForm)
        VideoModesList: TListBox;
        Button1: TButton;
      private
        { Private declarations }
      public
        { Public declarations }
      end;

  var
    SelectVideoModeForm: TSelectVideoModeForm;

implementation

  {$R *.DFM}

end.
