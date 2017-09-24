unit Debug;

interface

uses
  SysUtils;

//{$define _NODEBUG}

const
  Debugging = {$ifndef _NODEBUG} true {$else} false {$endif};
  Logging   = Debugging and true;

{$ifndef _NODEBUG}

const
  BreakOnError  = false;
  SaveToLogFile = true;
  LogTime       = true;

type
  EDebug =
    class(Exception)
      destructor Destroy;   override;
    end;

{$else}

type
  EDebug = class(Exception);

{$endif}


procedure Assert(cond : boolean; const msg : string);
procedure DebugError(const msg : string);
procedure LogThis(const msg : string);
procedure BrowseObject(obj : TObject);
procedure UnbrowseObject(obj : TObject);
procedure WriteDebugStr(which : string);     // protected
procedure DebugBreakPoint;


implementation

{$ifndef _NODEBUG}

uses
  Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, StrUtils, ComCtrls;


type
  TLogForm =
    class(TForm)
      public
        constructor CreateNew(aOwner : TComponent);
        destructor  Destroy; override;
      public
        procedure WriteStrs(const msgs : array of string);
        procedure WriteStr(const msg : string);
      protected
        procedure Resize;  override;
        procedure SaveToFile;
      private
        Messages : TRichEdit;
        procedure MessagesDblClick(Sender: TObject);
        procedure MessagesKeyDown(Sender : TObject; var Key : word; Shift : TShiftState);
        procedure OnLogDestroy(Sender : TObject);
    end;

type
  TBrowser =
    class
      public
        constructor CreateNew(aOwner : TComponent);
        destructor  Destroy; override;
      public
        procedure Browse(obj : TObject);
        procedure Unbrowse(obj : TObject);
      private
        //Objects : TList;
        //procedure FreeList;
    end;

var
  LogForm : TLogForm = nil;
  Browser : TBrowser = nil;

// TLogForm

constructor TLogForm.CreateNew(aOwner : TComponent);
  begin
    inherited;
    OnDestroy := OnLogDestroy;
    Width     := Screen.Width;
    Height    := Screen.Height div 5;
    Messages  := TRichEdit.Create(Self);
    with Messages do
      begin
        Align      := alClient;
        OnDblClick := MessagesDblClick;
        OnKeyDown  := MessagesKeyDown;
        ScrollBars := ssBoth;
        WordWrap   := false;
        PlainText  := true;
        Parent     := Self;
        Lines.Add('*** ' + UpperCase(ParamStr(0)) + ' LOG FILE. ' + DateTimeToStr(Now) + ' ***'^M^J);
      end;
  end;

destructor TLogForm.Destroy;
  begin
    if LogForm = Self
      then LogForm := nil;
    inherited;
  end;

procedure TLogForm.WriteStrs(const msgs : array of string);
  var
    i : integer;
    s : string;
  begin
    s := msgs[low(msgs)];
    for i := succ(low(msgs)) to high(msgs) do
      s := s + msgs[i];
    WriteStr(s);
  end;

procedure TLogForm.WriteStr(const msg : string);
  begin
    Messages.Lines.Add(msg);
  end;

procedure TLogForm.Resize;
  const
    ButtonsAreaHeight = 70;
  begin
    inherited;
    Messages.Height := Height - ButtonsAreaHeight;
  end;

procedure TLogForm.SaveToFile;
  begin
    if SaveToLogFile
      then Messages.Lines.SaveToFile(ChangeFileExt(ParamStr(0), '.LOG'));
  end;

procedure TLogForm.MessagesDblClick(Sender: TObject);
  begin
    Messages.Clear;
  end;

procedure TLogForm.MessagesKeyDown(Sender : TObject; var Key : word; Shift : TShiftState);
  begin
    case Key of
      ord('S') :
        if ssCtrl in Shift
          then SaveToFile;
    end;
  end;

procedure TLogForm.OnLogDestroy(Sender : TObject);
  begin
    SaveToFile;
  end;


// TBrowser

constructor TBrowser.CreateNew(aOwner : TComponent);
  begin
    inherited;
  end;

destructor TBrowser.Destroy;
  begin
    if Browser = Self
      then Browser := nil;
    inherited;
  end;

procedure TBrowser.Browse(obj : TObject);
  begin
  end;

procedure TBrowser.Unbrowse(obj : TObject);
  begin
  end;
(*
procedure TBrowser.FreeList;
  begin
  end;
*)

// Misc

procedure WriteDebugStr(which : string);
  begin
    OutputDebugString(pchar(which));
    if IsConsole
      then writeln(which)
      else
        begin
          if LogForm = nil
            then LogForm := TLogForm.CreateNew(Application);
          if not LogForm.Showing
            then LogForm.Show;
          LogForm.WriteStr(which);
        end;
  end;

procedure DebugBreakPoint;
  begin
    DebugBreak;
  end;

procedure LocalLog(const logo, msg : string);
  var
    s : string;
  begin
    if LogTime
      then s := FormatDateTime('hh:nn:ss <', Time)
      else s := '';
     s := s + logo + '> ' + msg;
    WriteDebugStr(s);
  end;

procedure Assert(cond : boolean; const msg : string);
  begin
    if not cond
      then
        begin
          LocalLog('A', msg);
          if BreakOnError
            then DebugBreak;
        end;
  end;

procedure DebugError(const msg : string);
  begin
    LocalLog('E', msg);
    if BreakOnError
      then DebugBreak;
  end;

procedure LogThis(const msg : string);
  begin
    LocalLog('L', msg);
  end;

procedure BrowseObject(obj : TObject);
  begin
  end;

procedure UnbrowseObject(obj : TObject);
  begin
  end;


// EDebug

destructor EDebug.Destroy;
  begin
    LocalLog('X', 'Destroying: ' + Message);
    inherited;
  end;

{$else}

procedure WriteDebugStr(which : string);
  begin
  end;

procedure DebugBreakPoint;
  begin
  end;

procedure DebugError(const msg : string);
  begin
  end;
  
procedure LocalLog(const logo, msg : string);
  begin
  end;

procedure Assert(cond : boolean; const msg : string);
  begin
  end;

procedure LogThis(const msg : string);
  begin
  end;

procedure BrowseObject(obj : TObject);
  begin
  end;

procedure UnbrowseObject(obj : TObject);
  begin
  end;

{$endif}

end.
