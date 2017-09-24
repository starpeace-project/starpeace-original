unit TestClientMain;

{$M+}

interface

  uses
    ShareMem,
    Windows,
    Messages,
    SysUtils,
    Classes,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    StdCtrls;

  type
    TTestMyDispatch = class(TForm)
        Button1: TButton;
        FormLeft: TEdit;
        TestFunRes: TEdit;
        WideStrProp: TEdit;
        procedure FormCreate(Sender: TObject);
        procedure ExecQueryClick(Sender: TObject);
      private
        { Private declarations }
        fObjectProxy   : variant;
        fRDOConnection : variant;
      public
        { Public declarations }
    end;

  var
    TestMyDispatch: TTestMyDispatch;

implementation

  {$R *.DFM}

  uses
    LogFile, ComObj;

  procedure TTestMyDispatch.FormCreate(Sender: TObject);
    begin
      fRDOConnection := CreateOLEObject( 'RDOClient.WinSockRDOConnection' );
      fRDOConnection.Server := '71.8.10.0';
      fRDOConnection.Port := 5000;
      if fRDOConnection.Connect( 10000 )
        then
          Application.MessageBox( 'Connection sucessfully established', 'TestClient', MB_OK )
        else
          begin
            Application.MessageBox( 'Error establishing connection', 'TestClient', MB_OK );
            Application.Terminate
          end;
      fObjectProxy := CreateOLEObject( 'RDOClient.RDOObjectProxy' );
      fObjectProxy.SetConnection( fRDOConnection );
      fObjectProxy.WaitForAnswer := true
    end;

  procedure TTestMyDispatch.ExecQueryClick(Sender: TObject);
    var
      NewLeft  : integer;
      ObjLeft  : integer;
      theHint  : string;
      adouble  : double;
      aStr     : string;
    begin
      if fObjectProxy.BindTo( 'Form' )
        then
          begin
            NewLeft := 10;
            fObjectProxy.Left := NewLeft;
            fObjectProxy.Top := 15;
            fObjectProxy.Width := 700;
            fObjectProxy.Height := 400;
            fObjectProxy.Caption := 'A test with my IDispatch';
            ObjLeft := fObjectProxy.Left;
            FormLeft.Text := IntToStr( ObjLeft );
            theHint := 'Could do it';
            fObjectProxy.Hint := theHint;
            if fObjectProxy.BindTo( 'Object' )
              then
                begin
                  fObjectProxy.TestProc( 'This is the string I want in the test file' );
                  adouble := 3.14;
                  aStr := 'Test method called successfully';
                  aStr := fObjectProxy.TestMethod( aStr, adouble, 10 );
                  TestFunRes.Text := aStr;
                  fObjectProxy.WStrProp := 'Testing wide char properties';
                  WideStrProp.Text := fObjectProxy.WStrProp;
                  fObjectProxy.IsSet := true;
                  if fObjectProxy.IsSet
                    then
                      Application.MessageBox( 'Property was set', 'TestClient', MB_OK )
                end
              else
                Application.MessageBox( 'Can not get to TestObject', 'TestClient', MB_OK )
          end
        else
          Application.MessageBox( 'Can not get to Form', 'TestClient', MB_OK )
    end;

end.
