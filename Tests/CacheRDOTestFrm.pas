unit CacheRDOTestFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, FramedButton, ExtCtrls, GradientBox, PDTabControl,
  TiledPanel;

type
  TForm1 = class(TForm)
    xfer_xPos: TSpinEdit;
    xfer_yPos: TSpinEdit;
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    PropName: TEdit;
    Label3: TLabel;
    xfer_Name: TEdit;
    Button2: TButton;
    Label4: TLabel;
    Itr: TButton;
    lbObjects: TListBox;
    xfer_Creator: TButton;
    xfer_ObjectId: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ItrClick(Sender: TObject);
    procedure lbObjectsClick(Sender: TObject);
    procedure xfer_CreatorClick(Sender: TObject);
    procedure FramedButton2Click(Sender: TObject);
  private
    fCSProxy  : OleVariant;
    fCacheObj : OleVariant;
  end;

var
  Form1: TForm1;

implementation

  uses
    ActiveX, ComObj, RDOInterfaces, WinSockRDOConnectionsServer,
    WinSockRDOConnection, RDOObjectProxy, CacheCommon, FiveViewUtils;

{$R *.DFM}

  procedure TForm1.Button1Click(Sender: TObject);
    var
      ClientConn : IRDOConnectionInit;
      ObjImg     : integer;
    begin
      try
        ClientConn := TWinSockRDOConnection.Create;
        ClientConn.Server := 'kim';
        ClientConn.Port   := 6000;
        if ClientConn.Connect(10000)
          then
            begin
              fCSProxy := TRDOObjectProxy.Create as IDispatch;
              fCSProxy.SetConnection(ClientConn);
              fCSProxy.BindTo(WSObjectCacherName);
              fCSProxy.TimeOut := 5000;
              {
              ObjImg := fCSProxy.CreateObject(WideString('Zyrane'));
              if ObjImg <> 0
                then
                  begin
                    fCacheObj := TRDOObjectProxy.Create as IDispatch;
                    fCacheObj.SetConnection(ClientConn);
                    fCacheObj.BindTo(ObjImg);
                    if fCacheObj.EnableReCache(false)
                      then
                        if fCacheObj.SetObject(xfer_xPos.Value, xfer_yPos.Value)
                          then
                            begin
                              xfer_Name.Text := fCacheObj.Properties(WideString(PropName.Text));
                              Caption        := WideString(fCacheObj.GetPath(integer(0)));
                            end
                          else ShowMessage('Niet!!! Can not connect to the Object in (' + xfer_xPos.Text + ', ' + xfer_yPos.Text + ')');
                  end
              }
            end
          else ShowMessage('Niet!!! Can not connect to the server...');
      except
      end;
    end;

  procedure TForm1.Button2Click(Sender: TObject);
    begin
      xfer_Name.Text := fCacheObj.Properties(WideString(PropName.Text));
    end;

  procedure TForm1.ItrClick(Sender: TObject);
    var
      Itr : integer;
      //Id  : integer;
    begin
      Itr := fCacheObj.GetIterator('Inputs');
      if Itr = 0
        then ShowMessage('Niet!')
        else
          begin
            //Id := fCacheObj.Id;
            fCacheObj.BindTo(Itr);
            if not fCacheObj.Empty(0)
              then
                repeat
                  lbObjects.Items.Add(WideString(fCacheObj.CurrentPath(0)));
                until fCacheObj.Next(0) = false;
            //fCacheObj.BindTo(Id);
          end;
    end;

  procedure TForm1.lbObjectsClick(Sender: TObject);
    begin
      lbObjects.Items.Clear;
    end;

  procedure TForm1.xfer_CreatorClick(Sender: TObject);
    begin
      try
        showmessage(fCSProxy.SimTimeOut(10000));
      except
      end;
      showmessage(fCSProxy.SimTimeOut(1000));
    end;

  procedure TForm1.FramedButton2Click(Sender: TObject);
    begin
      //FramedButton1.Enabled := not FramedButton1.Enabled;
    end;

end.
