unit VCLUtils;

// Copyright (c) 1996 Jorge Romero Gomez, Merchise.

interface

  uses
    Windows, Forms, Controls, WinUtils, comctrls, Classes;

  const
    {$EXTERNALSYM WS_EX_LAYERED}
    WS_EX_LAYERED = $00080000;

  const
  {$EXTERNALSYM LWA_COLORKEY}
    LWA_COLORKEY = $00000001;
  {$EXTERNALSYM LWA_ALPHA}
    LWA_ALPHA = $00000002;

  {$EXTERNALSYM ULW_COLORKEY}
    ULW_COLORKEY = $00000001;
  {$EXTERNALSYM ULW_ALPHA}
    ULW_ALPHA = $00000002;
  {$EXTERNALSYM ULW_OPAQUE}
    ULW_OPAQUE = $00000004;

  type
    TSingleInterfaced =  //.rag
      class(TInterfacedObject)
        public
          procedure AfterConstruction; override;
          procedure BeforeDestruction; override;
        end;

    TDebugInterfaced =  //.rag
      class(TObject, IUnknown)
        protected
          FRefCount: Integer;
          function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
          function _AddRef: Integer; stdcall;
          function _Release: Integer; stdcall;
        public
          procedure AfterConstruction; override;
          procedure BeforeDestruction; override;
          class function NewInstance: TObject; override;
          property RefCount: Integer read FRefCount;
      end;

type
  TSetLayeredWindowAttributes = function (Hwnd: THandle; crKey: COLORREF; bAlpha: Byte; dwFlags: DWORD): Boolean; stdcall;

var
  SetLayeredWindowAttributes: TSetLayeredWindowAttributes = nil;

  function  ParentRect( aControl : TControl ) : TRect; // Returns the rectangle of this control in parent's coordinates
  function  ScreenRect( aControl : TControl ) : TRect; // Returns the rectangle this control is using in screen coordinates

  procedure AvoidResizing( aForm : TForm );            // Disable resizing of this form
  procedure AllowResizing( aForm : TForm );            // Allow resizing of this form

  procedure HideFromTaskbar;                           // Remove the application from the taskbar
  procedure ShowOnTaskbar;                             // Show the application again in the taskbar

  function UniqueComponentName( Owner : TComponent; const NamePrefix : string ) : string;
  // eg: UniqueComponentName( Owner, 'Tool' ) = 'Tool3' if 'Tool1' and 'Tool2' already exist in Owner
  procedure ListViewShort(Listview: TListView; const Column: integer; const SortDescending: boolean);

  procedure FreememAndNil(var Po);  //.rag
  procedure RemoveComponentFreeAndNil(var ComObj);  //.rag
  procedure FreeTList(var aList);  //.rag
  procedure FreeTStrings(var aStrings);  //.rag
  procedure SetFollowControl(const ct1, ct2: TControl);

implementation

  uses
    SysUtils, mr_StrUtils, Math;

  type
    EDebugInterfacedExeption = Exception;

  function UniqueComponentName( Owner : TComponent; const NamePrefix : string ) : string;
    var
      i, FirstFreeIndx    : integer;
    begin
      with Owner do
        begin
          FirstFreeIndx := 1;
          i             := 0;
          while i < ComponentCount do
            if IndxOf( Components[i].Name, NamePrefix ) = FirstFreeIndx
              then
                begin
                  inc( FirstFreeIndx );
                  i := 0; // Start over
                end
              else inc( i );
        end;
      Result := NamePrefix + IntToStr( FirstFreeIndx );
    end;

  procedure HideFromTaskbar;
    begin
      HideWindow( Application.Handle );
    end;

  procedure ShowOnTaskbar;
    begin
      ShowDontActivateWindow( Application.Handle );
    end;

  function ParentRect( aControl : TControl ) : TRect;
    begin
      with aControl do
        begin
          Result := ClientRect;
          if Assigned( Parent )
            then
              begin
                with ClientOrigin do
                  OffsetRect( Result, x, y );
                with Parent.ClientOrigin do
                  OffsetRect( Result, -x, -y );
              end;
        end;
    end;

  function ScreenRect( aControl : TControl ) : TRect;
    begin
      with aControl, ClientOrigin do
        begin
          Result := ClientRect;
          OffsetRect( Result, x, y );
        end;
    end;

  procedure AvoidResizing( aForm : TForm );
    begin
      SetWindowSizeable( aForm.Handle, false );
    end;

  procedure AllowResizing( aForm : TForm );
    begin
      SetWindowSizeable( aForm.Handle, true  );
    end;

  type
    TListViewShort = 
      class
        class procedure ListViewCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
      end;

  class procedure TListViewShort.ListViewCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer; var Compare: Integer);
    begin
    end;
      
  procedure ListViewShort(Listview: TListView; const Column: integer; const SortDescending: boolean);
    begin
      Listview.OnCompare := TListViewShort.ListViewCompare;
      Listview.AlphaSort;
    end;

  procedure FreememAndNil(var Po);
    var
      P: pointer absolute Po;
    begin
      if P<>nil
        then 
          begin
            freemem(p);
            p := nil;
          end;
    end;

  procedure RemoveComponentFreeAndNil(var ComObj);
    var
      p : TControl;
    begin
      P := TControl(ComObj);
      TControl(ComObj) := nil;
      if p<>nil
        then
          begin
            p.Parent := nil;
            if p.Owner<>nil
              then p.Owner.RemoveComponent(p);
            P.Free;
          end;
    end;
    

  procedure FreeTList(var aList);  //.rag
    var
      List : TList absolute aList;
      i : integer;
      tmp : TList;
    begin
      if List <> nil
        then
          begin
            tmp := List;
            List := nil;
            with tmp do
              begin
                for i := 0 to pred(Count) do
                  TObject(Items[i]).Free;
                Free;
              end;
          end;
    end;

  procedure FreeTStrings(var aStrings);  //.rag
    var
      Strings: TStrings absolute aStrings;
      i : integer;
      tmp : TStrings;
    begin
      if Strings <> nil
        then
          begin
            tmp := Strings;
            Strings := nil;
            with tmp do
              begin
                for i := 0 to pred(Count) do
                  Objects[i].Free;
                Free;
              end;
          end;
    end;
    
{ TSingleInterfaced }

procedure TSingleInterfaced.AfterConstruction;
  begin
    inherited; // fRefCount := $7ffffff;
  end;

procedure TSingleInterfaced.BeforeDestruction;
  begin
    // inherited;
  end;

{ TDebugInterfaced }

function TDebugInterfaced._AddRef: Integer;
  begin
    Result := InterlockedIncrement(FRefCount);
  end;

function TDebugInterfaced._Release: Integer;
  begin
    Result := InterlockedDecrement(FRefCount);
    if Result = 0 
      then Destroy;
  end;

procedure TDebugInterfaced.AfterConstruction;
  begin
    InterlockedDecrement(FRefCount);
  end;

procedure TDebugInterfaced.BeforeDestruction;
  begin
    if RefCount <> 0 
      then raise EDebugInterfacedExeption.create(format('BeforeDestruction %d', [RefCount]));
  end;

class function TDebugInterfaced.NewInstance: TObject;
  begin
    Result := inherited NewInstance;
    TDebugInterfaced(Result).FRefCount := 1;
  end;

function TDebugInterfaced.QueryInterface(const IID: TGUID;out Obj): HResult;
  const
    E_NOINTERFACE = HResult($80004002);
  begin
    if GetInterface(IID, Obj) 
      then Result := 0 
      else Result := E_NOINTERFACE;
  end;

function EncryptDecryptWithKey(Value, Key:string): string; 
  var 
    i : Integer; 
    j : Integer; 
    b : boolean;
  begin 
    setlength(result, Max(length(value), length(key)));
    fillchar(result[1], length(result), 0);
    move(value[1], result[1], length(value));
    j := length(key);
    i := 1;
    b := true;
    while i<=Length(result) do
      begin
        if b and (result[i]=#0)
          then b:= false
          else result[i] := char(128-i);
        inc(i);
        result[i] := char(byte(result[i]) xor byte(Key[j])); 
        dec(j); 
        if j<=0
          then j := length(key);
      end; 
  end;  

procedure SetFollowControl(const ct1, ct2: TControl);
  begin
//    ct1.AutoSize := true;
    ct2.left := ct1.left + ct1.Width + 5;
  end;

procedure InitProcs;
const
  sUser32 = 'User32.dll';
var
  ModH: HMODULE;
begin
  ModH := GetModuleHandle(sUser32);
  if ModH <> 0 then
     @SetLayeredWindowAttributes := GetProcAddress(ModH, 'SetLayeredWindowAttributes');
end;

initialization
  InitProcs;
finalization
end.
