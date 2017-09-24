unit BackupFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    function CompFuction(o1, o2 : TObject) : integer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  BackupInterfaces, BackupObjects, Collection, CollectionBackup;

type
  TX = class;
  TY = class;

{$M+}
  TX =
    class
      public
        X : string;
        Y : TY;
        E : TNotifyEvent;
        constructor Create(aY : TY);
    end;

  TY =
    class
      public
        Y : string;
        X : TX;
        E : TNotifyEvent;
        constructor Create(aX : TX);
      published
        procedure XE(O : TObject);
    end;
{$M-}

constructor TX.Create(aY : TY);
  begin
    X := 'X Instance...';
    Y := aY;
    E := Form1.Button1Click;
  end;

constructor TY.Create(aX : TX);
  begin
    Y := 'Y Instance...';
    X := aX;
  end;

procedure TY.XE(O : TObject);
  begin
    ShowMessage(Y);
  end;

type
  TXBackupAgent =
    class(TBackupAgent)
      protected
        class procedure Write(Stream : IBackupWriter; Obj : TObject); override;
        class procedure Read (Stream : IBackupReader; Obj : TObject); override;
    end;

type
  TYBackupAgent =
    class(TBackupAgent)
      protected
        class procedure Write(Stream : IBackupWriter; Obj : TObject); override;
        class procedure Read (Stream : IBackupReader; Obj : TObject); override;
    end;

// TXBackupAgent

class procedure TXBackupAgent.Write(Stream : IBackupWriter; Obj : TObject);
  begin
    with TX(Obj) do
      begin
        Stream.WriteString('X', X);
        Stream.WriteObject('Y', Y);
        Stream.WriteMethod('E', TMethod(E));
      end;
  end;

class procedure TXBackupAgent.Read(Stream : IBackupReader; Obj : TObject);
  begin
    with TX(Obj) do
      begin
        X := Stream.ReadString('X', '');
        Stream.ReadObject('Y', TObject(Y), nil);
        Stream.ReadMethod('E', TMethod(E), NULLPROC);
      end;
  end;

// TYBackupAgent

class procedure TYBackupAgent.Write(Stream : IBackupWriter; Obj : TObject);
  begin
    with TY(Obj) do
      begin
        Stream.WriteString('Y', Y);
        Stream.WriteObjectRef('X', X);
      end;
  end;

class procedure TYBackupAgent.Read(Stream : IBackupReader; Obj : TObject);
  begin
    with TY(Obj) do
      begin
        Y := Stream.ReadString('Y', '');
        Stream.ReadObject('X', TObject(X), nil);
      end;
  end;

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
  var
    St : IBackupWriter;
    X : TX;
    Y : TY;
  begin
    St := TVerboseBackupWriter.Create(TFileStream.Create('c:\temp\x.txt', fmCreate), true, nil);
    X  := TX.Create(nil);
    Y  := TY.Create(X);
    X.E := Y.XE;
    X.Y := Y;
    St.WriteObject('X', X);
    St.WriteObjectRef('Y', Y);
  end;

procedure TForm1.Button2Click(Sender: TObject);
  var
    St : IBackupReader;
    X  : TX;
    Y  : TY;
  begin
    St := TVerboseBackupReader.Create(TFileStream.Create('c:\temp\x.txt', fmOpenRead), 'c:\temp\');
    try
     St.ReadObject('X', TObject(X), nil);
     St.ReadObject('Y', TObject(Y), nil);
    finally
    end;
  end;

function TForm1.CompFuction(o1, o2 : TObject) : integer;
  begin
    if o1 = o2
      then result := 0
      else
        if integer(o1) > integer(o2)
          then result := 1
          else result := -1;
  end;

initialization

  TXBackupAgent.Register([TX, TForm1]);
  TYBackupAgent.Register([TY]);
  CollectionBackup.RegisterBackup;

end.
