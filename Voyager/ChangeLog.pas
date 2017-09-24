unit ChangeLog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Config,
  FramedButton, StdCtrls, ComCtrls, ExtCtrls, GradientBox,
  InternationalizerComponent;

type
  TChangeLogView =
    class(TForm)
        Panel1: TPanel;
        Panel2: TPanel;
        Log: TRichEdit;
        GradientBox1: TGradientBox;
        IKnowBtn: TFramedButton;
        LaterBtn: TFramedButton;
        header: TLabel;
        InternationalizerComponent1: TInternationalizerComponent;
        Panel3: TPanel;
        Panel4: TPanel;
        Panel5: TPanel;
        Panel6: TPanel;
        procedure IKnowBtnClick(Sender: TObject);
        procedure LaterBtnClick(Sender: TObject);
        procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
      private
        fLogPath    : string;
        fLastEntry  : integer;
        fNewEntries : boolean;
      public
        property LastEntry  : integer read fLastEntry   write fLastEntry;
        property LogPath    : string  read fLogPath     write fLogPath;
        property NewEntries : boolean read fNewEntries;
      public
        procedure RenderNews;
      protected
        procedure SetParent(which : TWinControl);  override;
        procedure WMHitTest(var Message : TMessage);
    end;

var
  ChangeLogView: TChangeLogView;

implementation

  uses
    MathUtils, CoolSB;

{$R *.DFM}

  procedure TChangeLogView.SetParent(which : TWinControl);
    begin
      inherited;
      if InitSkinImage and (which<>nil)
        then
          begin
            InitializeCoolSB(Log.Handle);
            if hThemeLib <> 0
              then
                SetWindowTheme(Log.Handle, ' ', ' ');
            CoolSBEnableBar(Log.Handle, FALSE, TRUE);
          end;
    end;

  procedure TChangeLogView.RenderNews;
    var
      Entry        : integer;
      NewEntries   : integer;
      FirstEntry   : integer;
      LastEntryIdx : integer;
      i            : integer;
    begin
      if InitSkinImage
        then
          begin
            InitializeCoolSB(Log.Handle);
            if hThemeLib <> 0
              then
                SetWindowTheme(Log.Handle, ' ', ' ');
            CoolSBEnableBar(Log.Handle, FALSE, TRUE);
          end;
      Log.Lines.LoadFromFile( fLogPath );
      i := 0;
      NewEntries := 0;
      LastEntryIdx := -1;
      FirstEntry   := -1;
      while (i < Log.Lines.Count) and (LastEntryIdx = -1) do
        begin
          if pos('{', Log.Lines[i]) > 0
            then
              begin
                Entry := StrToInt( copy(Log.Lines[i], 2, length(Log.Lines[i]) - 1) );
                if Entry <= fLastEntry
                  then LastEntryIdx := i
                  else
                    begin
                      inc( NewEntries );
                      if FirstEntry = -1
                        then FirstEntry := Entry;
                    end;
              end;
          inc( i );
        end;
      if LastEntryIdx > -1
        then
          for i := LastEntryIdx to pred(Log.Lines.Count) do
            Log.Lines.Delete( LastEntryIdx );
      for i := pred(Log.Lines.Count) downto 0 do
        if pos('{', Log.Lines[i]) > 0
          then Log.Lines.Delete( i );
      Log.SelStart := 0;
      fNewEntries := NewEntries > 0;
      fLastEntry  := FirstEntry;
      {
      Entry := 0;
      Log.Lines.LoadFromFile( fLogPath );
      LastEntryIdx := pred(Log.Lines.Count);
      MarkCnt      := 0;
      for i := pred(Log.Lines.Count) downto 0 do
        try
          if (Log.Lines[i] <> '') and (Log.Lines[i][1] = '{')
            then
              begin
                Entry := StrToInt( copy(Log.Lines[i], 2, length(Log.Lines[i]) - 1) );
                if Entry <= fLastEntry
                  then LastEntryIdx := i
                  else inc( MarkCnt );
              end;
        except
        end;
      for i := LastEntryIdx to pred(Log.Lines.Count) do
        Log.Lines.Delete( LastEntryIdx );
      for i := pred(Log.Lines.Count) downto 0 do
        if (Log.Lines[i] <> '') and (Log.Lines[i][1] = '{')
          then Log.Lines.Delete( i );
      LastEntry := max( LastEntry, Entry );
      }
    end;

  procedure TChangeLogView.FormMouseMove(Sender: TObject; Shift: TShiftState;
    X, Y: Integer);
  const
    SC_DragMove = $F012;
  begin
    ReleaseCapture;
    perform(WM_SysCommand, SC_DragMove, 0);
  end;

  procedure TChangeLogView.WMHitTest(var Message : TMessage);
  begin
    Message.Result := HTCAPTION;
  end;

  procedure TChangeLogView.IKnowBtnClick(Sender: TObject);
    begin
      ModalResult := mrOk;
    end;

  procedure TChangeLogView.LaterBtnClick(Sender: TObject);
    begin
      ModalResult := mrCancel;
    end;

end.
