unit TestServerMain;

{$M+}

interface

  uses
    ShareMem, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    StdCtrls, RDOInterfaces, WinSockRDOConnectionsServer, RDOServer;

  type
    TTestMyDispatch = class(TForm)
        TestFloat: TEdit;
        TestInteger: TEdit;
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
      private
        { Private declarations }
        fRDOConnectionServer : IRDOConnectionsServer;
        fRDOServer           : TRDOServer;
      public
        { Public declarations }
    end;

  var
    TestMyDispatch: TTestMyDispatch;

implementation

  {$R *.DFM}

  uses
    LogFile;

  type
    TTestParent =
      class
        private
          fIsSet : boolean;
          fChar  : char;
          fWideStr : widestring;
        published
          function TestMethod( TestStr : widestring; Testdouble : double; TestInteger : integer ) : variant; virtual;
          procedure TestProc( aWStr : widestring );
          procedure Test;
          function TestVar( out ByRefStr, ByRefInt, ByRefFloat : variant ) : variant;
          property IsSet    : boolean    read fIsSet   write fIsSet;
          property CharProp : char       read fChar    write fChar;
          property WStrProp : widestring read fWideStr write fWideStr;
      end;

    TTestObject =
      class( TTestParent )
        published
          function TestMethod( TestStr : widestring; Testdouble : double; TestInteger : integer ) : variant; override;
      end;

  var
    TestObj : TTestObject;

  function TTestParent.TestMethod( TestStr : widestring; Testdouble : double; TestInteger : integer ) : variant;
    begin
      Result := 'TTestParent';
    end;

  function TTestObject.TestMethod( TestStr : widestring; Testdouble : double; TestInteger : integer ) : variant;
    begin
      TestMyDispatch.Caption := TestStr;
      TestMyDispatch.TestFloat.Text := FloatToStr( Testdouble );
      TestMyDispatch.TestInteger.Text := IntToStr( TestInteger );
      Result := inherited TestMethod( TestStr, Testdouble, TestInteger );
      Result := Result + ' TTestObject'
    end;

  procedure TTestParent.TestProc( aWStr : widestring );
    var
      aTextFile : textfile;
      aStr      : string;
    begin
      AssignFile( aTextFile, 'test.txt' );
      ReWrite( aTextFile );
      aStr := aWStr;
      WriteLn( aTextFile, aStr );
      CloseFile( aTextFile )
    end;

  procedure TTestParent.Test;
    begin
      TestMyDispatch.Caption := 'test got called'
    end;

  function TTestParent.TestVar( out ByRefStr, ByRefInt, ByRefFloat : variant ) : variant;
    begin
      ByRefStr := 'The byrefs work';
      ByRefInt := 10;
      ByRefFloat := 2.77;
      Result := 'Result'
    end;

  procedure TTestMyDispatch.FormCreate(Sender: TObject);
    begin
      try
        fRDOConnectionServer := TWinSockRDOConnectionsServer.Create( 5000 );
        fRDOServer := TRDOServer.Create( fRDOConnectionServer as IRDOServerConnection );
        fRDOServer.RegisterObject( 'Form', integer( Self ) );
        TestObj := TTestObject.Create;
        fRDOServer.RegisterObject( 'Object', integer( TestObj ) );
        fRDOConnectionServer.StartListening;
        SetLogFile( 'TestServer.Log' )
      except
        Application.MessageBox( 'Error initializing RDO', 'TestServer', MB_OK )
      end
    end;

  procedure TTestMyDispatch.FormDestroy(Sender: TObject);
    begin
      TestObj.Free;
      fRDOServer.Free
    end;

end.
