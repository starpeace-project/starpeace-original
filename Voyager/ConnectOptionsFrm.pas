unit ConnectOptionsFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, GMList, Menus;

type
  TGMConnOptions =
    class(TForm)
        Panel2: TPanel;
        GMView: TListView;
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
        procedure GMViewMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        procedure MoveDown1Click(Sender: TObject);
        procedure MoveDown2Click(Sender: TObject);
        procedure Highest1Click(Sender: TObject);
        procedure Normal1Click(Sender: TObject);
        procedure Ignored1Click(Sender: TObject);
      public
        procedure setGameMasterList( aGameMasterList : TGameMasterList );
      private
        fGameMasterList : TGameMasterList;
        procedure FillList;
    end;

implementation

{$R *.DFM}

  procedure TGMConnOptions.setGameMasterList( aGameMasterList : TGameMasterList );
    begin
      fGameMasterList := aGameMasterList;
      FillList;
    end;

  const
    PriorityToStr : array[low(TGMConnectOptions)..high(TGMConnectOptions)] of string = ( GetLiteral('Literal196'), GetLiteral('Literal197'), GetLiteral('Literal198') );

  procedure TGMConnOptions.FillList;
    var
      i    : integer;
      Item : TListItem;
    begin
      for i := 0 to pred(fGameMasterList.Count) do
        begin
          Item := GMView.Items.Add;
          with Item do
            begin
              Data    := fGameMasterList.Item[i];
              Caption := fGameMasterList.Item[i].Name;
              SubItems.Add( PriorityToStr[fGameMasterList.Item[i].Options] );
            end;
        end;
    end;

  procedure TGMConnOptions.GMViewMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    var
      Screen : TPoint;
    begin
      if (Button = mbRight) and (GMView.Selected <> nil)
        then
          begin
            Screen := ClientToScreen( Point( X, Y ) );
            PopupMenu1.Popup( Screen.X, Screen.Y );
          end;
    end;

  procedure TGMConnOptions.MoveDown1Click(Sender: TObject);
    var
      Info : TGameMasterListInfo;
      Item : TListItem;
      Idx  : integer;
    begin
      if GMView.Selected <> nil
        then
          begin
            Info := TGameMasterListInfo(GMView.Selected.Data);
            if fGameMasterList.MoveGameMasterUp( Info )
              then
                begin
                  GMView.Items.Clear;
                  FillList;
                end;
          end;
    end;

  procedure TGMConnOptions.MoveDown2Click(Sender: TObject);
    var
      Info : TGameMasterListInfo;
      Item : TListItem;
      Idx  : integer;
    begin
      if GMView.Selected <> nil
        then
          begin
            Info := TGameMasterListInfo(GMView.Selected.Data);
            if fGameMasterList.MoveGameMasterDown( Info )
              then
                begin
                  GMView.Items.Clear;
                  FillList;
                end;
          end;
    end;

  procedure TGMConnOptions.Highest1Click(Sender: TObject);
    var
      Info   : TGameMasterListInfo;
      Item   : TListItem;
      NewIdx : integer;
      Idx    : integer;
    begin
      Item := GMView.Selected;
      if Item <> nil
        then
          begin
            Info := TGameMasterListInfo(Item.Data);
            fGameMasterList.ChangeGameMasterProperties( Info, GMCO_HIGHPRIORITY, NewIdx );
            GMView.Items.Clear;
            FillList;
          end;
    end;

  procedure TGMConnOptions.Normal1Click(Sender: TObject);
    var
      Info   : TGameMasterListInfo;
      Item   : TListItem;
      NewIdx : integer;
      Idx    : integer;
    begin
      Item := GMView.Selected;
      if Item <> nil
        then
          begin
            Info := TGameMasterListInfo(Item.Data);
            fGameMasterList.ChangeGameMasterProperties( Info, GMCO_NORMALPRIORITY, NewIdx );
            GMView.Items.Clear;
            FillList;
          end;
    end;

  procedure TGMConnOptions.Ignored1Click(Sender: TObject);
    var
      Info   : TGameMasterListInfo;
      Item   : TListItem;
      NewIdx : integer;
      Idx    : integer;
    begin
      Item := GMView.Selected;
      if Item <> nil
        then
          begin
            Info := TGameMasterListInfo(Item.Data);
            fGameMasterList.ChangeGameMasterProperties( Info, GMCO_IGNORE, NewIdx );
            GMView.Items.Clear;
            FillList;
          end;
    end;

end.
