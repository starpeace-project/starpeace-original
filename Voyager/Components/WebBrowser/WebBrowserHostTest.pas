unit WebBrowserHostTest;

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    StdCtrls, CustomWebBrowser;

  type
    TWebBrowserForm =
      class(TForm)
          procedure FormShow(Sender: TObject);
        private
          { Private declarations }
          fWebBrowser : TCustomWebBrowser;
        public
          { Public declarations }
      end;

  var
    WebBrowserForm: TWebBrowserForm;

implementation

  uses
    ActiveX, LogFile;

  {$R *.DFM}

  procedure TWebBrowserForm.FormShow(Sender: TObject);
    var
      UseLess    : OleVariant;
      URL        : OleVariant;
      {
      OLEObjDisp : IDispatch;
      OLEObj     : IOLEObject;
      }
    begin
      SetLogFile('webbrowser.log');
      UseLess := NULL;
      fWebBrowser := TCustomWebBrowser.Create(Self);
      TControl(fWebBrowser).Parent := Self;
      fWebBrowser.Align := alClient;
      fWebBrowser.HideScrollBars := true;
      fWebBrowser.HideBorders := true;
      URL := InputBox('Customized IE 4.0', 'Enter HTTP &address you want to connect to: ', 'http://www.iosphere.net');
      //URL := 'C:';
      fWebBrowser.Navigate2(URL, UseLess, UseLess, UseLess, UseLess);
    end;

end.

