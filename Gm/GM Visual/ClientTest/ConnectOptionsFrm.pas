unit ConnectOptionsFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, GMList, Menus, Buttons, StdCtrls, FramedButton,
  InternationalizerComponent, ImgList;

type
  TGMConnOptions =
    class(TForm)
        ImageList1: TImageList;
        PopupMenu1: TPopupMenu;
        MoveDown1: TMenuItem;
        MoveDown2: TMenuItem;
        N1: TMenuItem;
        Priority1: TMenuItem;
        Highest1: TMenuItem;
        Normal1: TMenuItem;
        Ignored1: TMenuItem;
        N2: TMenuItem;
        Panel3: TPanel;
        Panel2: TPanel;
    GMList: TListView;
        Panel1: TPanel;
        bMoveUp: TFramedButton;
        bMoveDown: TFramedButton;
        bRemove: TFramedButton;
        bFavorite: TFramedButton;
        bNormal: TFramedButton;
        bIgnored: TFramedButton;
        Label8: TLabel;
        bClose: TFramedButton;
        Panel4: TPanel;
        Label1: TLabel;
    InternationalizerComponent1: TInternationalizerComponent;
        procedure GMViewMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        procedure MoveDown1Click(Sender: TObject);
        procedure MoveDown2Click(Sender: TObject);
        procedure Highest1Click(Sender: TObject);
        procedure Normal1Click(Sender: TObject);
        procedure Ignored1Click(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure GMListClick(Sender: TObject);
        procedure SpeedButton3Click(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure bCloseClick(Sender: TObject);
      private
        fInitialized    : boolean;
        fGameMasterList : TGameMasterList;
        procedure FillList;                                 
        procedure FocusItem( Item : TGameMasterListInfo );
      public
        property GameMasters : TGameMasterList write fGameMasterList;
    end;

implementation

  {$R *.DFM}

  uses
    CoolSB;

  const
    PriorityToStr : array[low(TGMConnectOptions)..high(TGMConnectOptions)] of string = ( 'Literal426', 'Literal427', 'Literal428' );

  const
    FAVORITE_IMGIDX = 0;
    NORMAL_IMGIDX   = 1;
    IGNORED_IMGIDX  = 2;

  const
    GMConnectOptionsToImgIdx : array[TGMConnectOptions] of integer = (FAVORITE_IMGIDX, NORMAL_IMGIDX, IGNORED_IMGIDX);

  procedure TGMConnOptions.FillList;
    var
      i    : integer;
      Item : TListItem;
    begin
      if fInitialized
        then
          begin
            if GMList <> nil
              then
                begin
                  GMList.Items.Clear;
                  for i := 0 to pred(fGameMasterList.Count) do
                    begin
                      Item := GMList.Items.Add;
                      with Item do
                        begin
                          Data       := fGameMasterList.Item[i];
                          ImageIndex := GMConnectOptionsToImgIdx[fGameMasterList.Item[i].Options];
                          Caption    := fGameMasterList.Item[i].Name;
                        end;
                    end;
                end;
          end;
    end;

  procedure TGMConnOptions.FocusItem( Item : TGameMasterListInfo );
    begin
      if Item <> nil
        then
          begin
            bFavorite.Enabled := true;
            bNormal.Enabled   := true;
            bIgnored.Enabled  := true;
            bMoveUp.Enabled   := true;
            bMoveDown.Enabled := true;
            bRemove.Enabled   := true;
            case Item.Options of
              GMCO_HIGHPRIORITY   : bFavorite.Selected := true;
              GMCO_NORMALPRIORITY : bNormal.Selected   := true;
              GMCO_IGNORE         : bIgnored.Selected  := true;
            end
          end
        else
          begin
            bFavorite.Enabled := false;
            bNormal.Enabled   := false;
            bIgnored.Enabled  := false;
            bMoveUp.Enabled   := false;
            bMoveDown.Enabled := false;
            bRemove.Enabled   := false;
          end;
    end;

  procedure TGMConnOptions.GMViewMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    var
      Screen : TPoint;
    begin
      if (Button = mbRight) and (GMList.Selected <> nil)
        then
          begin
            Screen := ClientToScreen( Point( X, Y ) );
            PopupMenu1.Popup( Screen.X, Screen.Y );
          end;
    end;

  procedure TGMConnOptions.MoveDown1Click(Sender: TObject);
    var
      Info : TGameMasterListInfo;
      Idx  : integer;
    begin
      if (GMList <> nil) and (GMList.Selected <> nil)
        then
          begin
            Info := TGameMasterListInfo(GMList.Selected.Data);
            Idx  := GMList.Selected.Index;
            if fGameMasterList.MoveGameMasterUp( Info )
              then
                begin
                  GMList.Items.Clear;
                  FillList;
                  GMList.Selected := GMList.Items[pred(Idx)];
                end;
          end;
    end;

  procedure TGMConnOptions.MoveDown2Click(Sender: TObject);
    var
      Info : TGameMasterListInfo;
      Idx  : integer;
    begin
      if (GMList <> nil) and (GMList.Selected <> nil)
        then
          begin
            Info := TGameMasterListInfo(GMList.Selected.Data);
            Idx  := GMList.Selected.Index;
            if fGameMasterList.MoveGameMasterDown( Info )
              then
                begin
                  GMList.Items.Clear;
                  FillList;
                  GMList.Selected := GMList.Items[succ(Idx)];
                end;
          end;
    end;

  procedure TGMConnOptions.Highest1Click(Sender: TObject);
    var
      Info   : TGameMasterListInfo;
      Item   : TListItem;
      NewIdx : integer;
    begin
      Item := GMList.Selected;
      if Item <> nil
        then
          begin
            Info := TGameMasterListInfo(Item.Data);
            fGameMasterList.ChangeGameMasterProperties( Info, GMCO_HIGHPRIORITY, NewIdx );
            GMList.Items.Clear;
            FillList;
            GMList.Selected := GMList.Items[NewIdx];
          end;
    end;

  procedure TGMConnOptions.Normal1Click(Sender: TObject);
    var
      Info   : TGameMasterListInfo;
      Item   : TListItem;
      NewIdx : integer;
    begin
      Item := GMList.Selected;
      if Item <> nil
        then
          begin
            Info := TGameMasterListInfo(Item.Data);
            fGameMasterList.ChangeGameMasterProperties( Info, GMCO_NORMALPRIORITY, NewIdx );
            GMList.Items.Clear;
            FillList;
            GMList.Selected := GMList.Items[NewIdx];
          end;
    end;

  procedure TGMConnOptions.Ignored1Click(Sender: TObject);
    var
      Info   : TGameMasterListInfo;
      Item   : TListItem;
      NewIdx : integer;
    begin
      Item := GMList.Selected;
      if Item <> nil
        then
          begin
            Info := TGameMasterListInfo(Item.Data);
            fGameMasterList.ChangeGameMasterProperties( Info, GMCO_IGNORE, NewIdx );
            GMList.Items.Clear;
            FillList;
            GMList.Selected := GMList.Items[NewIdx];
          end;
    end;

  procedure TGMConnOptions.FormCreate(Sender: TObject);
    {
    var
      Idx : integer;
    }
    begin
      fGameMasterList := TGameMasterList.Create;
      {
      fGameMasterList.AddGameMaster( 'mima', GMCO_IGNORE, Idx );
      fGameMasterList.AddGameMaster( 'pepe', GMCO_HIGHPRIORITY, Idx );
      fGameMasterList.AddGameMaster( 'taka', GMCO_NORMALPRIORITY, Idx );
      fGameMasterList.AddGameMaster( 'yuka', GMCO_NORMALPRIORITY, Idx );
      }
      //FillList;
    end;

  procedure TGMConnOptions.GMListClick(Sender: TObject);
    var
      Info : TGameMasterListInfo;
    begin
      if GMList.Selected <> nil
        then
          begin
            Info := TGameMasterListInfo(GMList.Selected.Data);
            FocusItem( Info );
          end                                                     
        else FocusItem( nil );
    end;

  procedure TGMConnOptions.SpeedButton3Click(Sender: TObject);
    var
      Info : TGameMasterListInfo;
    begin
      if GMList.Selected <> nil
        then
          begin
            Info := TGameMasterListInfo(GMList.Selected.Data);
            fGameMasterList.DeleteGameMaster( Info.Name );
            GMList.Items.Clear;
            FillList;
          end;
    end;

  procedure TGMConnOptions.FormShow(Sender: TObject);
    begin
      fInitialized := true;
      FillList;
      if (GMList <> nil)and (GMList.Selected = nil)
        then FocusItem( nil );
      if InitSkinImage 
        then InitializeCoolSB(GMList.Handle);
    end;


  procedure TGMConnOptions.bCloseClick(Sender: TObject);
    begin
      ModalResult := mrOk;
    end;


end.
