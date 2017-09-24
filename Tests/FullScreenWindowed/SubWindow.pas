unit SubWindow;

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    OleCtrls, SHDocVw, ComCtrls, StdCtrls;

  type
    TSubForm = class(TForm)
      TabControl1: TTabControl;
      WebBrowser: TWebBrowser;
      Button1: TButton;
      procedure FormCreate(Sender: TObject);
      procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    private
      { Private declarations }
    public
      { Public declarations }
    end;

  var
    SubForm: TSubForm;

implementation

  uses
    SubWindow2, FullScreenWindow;

  {$R *.DFM}

  procedure TSubForm.FormCreate(Sender: TObject);
    begin
      WebBrowser.Navigate('http://www.starpeace.net');
    end;

  procedure TSubForm.Button1Click(Sender: TObject);
    begin
      SubForm2.Left := 0;
      SubForm2.Top := 0;
      SubForm2.Parent := FullScreenForm;
      SubForm2.Show;
    end;

  procedure TSubForm.FormClose(Sender: TObject; var Action: TCloseAction);
    begin
      FullScreenForm.WindowClosed(Handle);
    end;

  procedure TSubForm.FormShow(Sender: TObject);
    begin
      FullScreenForm.WindowOpened(Handle);
    end;

end.
