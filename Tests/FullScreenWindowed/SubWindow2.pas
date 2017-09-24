unit SubWindow2;

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    StdCtrls;

  type
    TSubForm2 =
      class(TForm)
          Button1: TButton;
          Button2: TButton;
          Memo: TMemo;
          procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
        private
          { Private declarations }
        public
          { Public declarations }
      end;

  var
    SubForm2: TSubForm2;

implementation

  uses
    FullScreenWindow;

  {$R *.DFM}

  procedure TSubForm2.FormClose(Sender: TObject; var Action: TCloseAction);
    begin
      FullScreenForm.WindowClosed(Handle);
    end;

  procedure TSubForm2.FormShow(Sender: TObject);
    begin
      FullScreenForm.WindowOpened(Handle);
    end;

  procedure TSubForm2.Button1Click(Sender: TObject);
    begin
      Visible := false;
    end;

  procedure TSubForm2.Button2Click(Sender: TObject);
    begin
      Visible := true;
    end;

end.
