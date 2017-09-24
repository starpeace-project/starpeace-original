unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, MemoryManager, ExtCtrls, ComCtrls, ToolWin;

type
  TForm2 =
    class(TForm)
        ComboBox1: TComboBox;
        Label1: TLabel;
        Edit1: TEdit;
        Button1: TButton;
        Button2: TButton;
        MemoryManagerClass1: TMemoryManagerClass;
        Button3: TButton;
        Button4: TButton;
        Button5: TButton;
        RichEdit1: TRichEdit;
        ToolBar1: TToolBar;
        ToolButton1: TToolButton;
        ToolButton2: TToolButton;
        ToolButton3: TToolButton;
        ToolButton4: TToolButton;
        procedure ComboBox1Change(Sender: TObject);
        procedure Button1Click(Sender: TObject);
        procedure Button2Click(Sender: TObject);
        procedure Button3Click(Sender: TObject);
        procedure Button4Click(Sender: TObject);
        procedure Button5Click(Sender: TObject);
        procedure ToolButton1Click(Sender: TObject);
        procedure ToolButton2Click(Sender: TObject);
        procedure ToolButton4Click(Sender: TObject);
      private
        { Private declarations }
      public
        { Public declarations }
    end;

var
  Form2: TForm2;

implementation

{$R *.DFM}

procedure CreateObj; external 'DllPersistent.dll';
procedure SavePers; external 'DllPersistent.dll';

 
procedure TForm2.ComboBox1Change(Sender: TObject);
  var
    Sel : TMemoryShow;
  begin
    if comparetext(ComboBox1.Text, 'msMemCount')=0
      then Sel := msMemCount
      else
        if comparetext(ComboBox1.Text, 'msFreeMemCount')=0
          then Sel := msFreeMemCount
          else
            if comparetext(ComboBox1.Text, 'msReallocMemCount')=0
              then Sel := msReallocMemCount
              else
                if comparetext(ComboBox1.Text, 'msAllocMemCount')=0
                  then Sel := msAllocMemCount
                  else
                    if comparetext(ComboBox1.Text, 'msAllocMemSize')=0
                      then Sel := msAllocMemSize
                      else Sel := msPendingCall;

    MemoryManagerClass1.MemoryShow := sel;
  end;

procedure TForm2.Button1Click(Sender: TObject);
  begin
    MemoryManagerClass1.Interval := StrtoInt(Edit1.text);
  end;

procedure TForm2.Button2Click(Sender: TObject);
  begin
    MemoryManagerClass1.reset;
  end;

procedure TForm2.Button3Click(Sender: TObject);
  var
    p : pchar;
    q : pchar;
  begin
    getmem(p, 300);
    p[23]:= '3';
    getmem(q, 100);
    p[23] := 'a';
    ReallocMem(p, 800);
    freemem(q);
    freemem(p);
  end;

procedure TForm2.Button4Click(Sender: TObject);
  begin
    CreateObj;
  end;

procedure TForm2.Button5Click(Sender: TObject);
  var
    p : pointer;
  begin
//     p := VirtualAlloc(nil, minSize, MEM_RESERVE, PAGE_NOACCESS);

  end;
                                                
procedure TForm2.ToolButton1Click(Sender: TObject);
  var
    SystemInfo : TSystemInfo;
  begin
    GetSystemInfo(SystemInfo);
    with SystemInfo, richedit1.lines do
      begin
        add(format('wProcessorArchitecture = %d', [wProcessorArchitecture]));
        add(format('wReserved:= %d', [wReserved]));
        add(format('dwPageSize := %d', [dwPageSize]));
        add(format('lpMinimumApplicationAddress := %x', [integer(lpMinimumApplicationAddress)]));
        add(format('lpMaximumApplicationAddress:= %x', [integer(lpMaximumApplicationAddress)]));
        add(format('dwActiveProcessorMask := %d',[dwActiveProcessorMask]));
        add(format('dwNumberOfProcessors := %d', [dwNumberOfProcessors]));
        add(format('dwProcessorType := %d ',[ dwProcessorType]));
        add(format('dwAllocationGranularity:= %d ',[ dwAllocationGranularity]));
        add(format('wProcessorLevel:= %d ',[ wProcessorLevel]));
        add(format('wProcessorRevision:= %d ',[ wProcessorRevision]));
      end;
  end;

procedure TForm2.ToolButton2Click(Sender: TObject);
  const
    cSpaceMin = 1024*1024;
  var
    lpvAddr: pointer;
    dwPageSize: DWORD;
    bLocked: BOOL;    
    sSysInfo: TSystemInfo;
    p : pointer;
  begin
    GetSystemInfo(sSysInfo);
    with richedit1.lines do
      begin
        add(format('This computer has a page size of %d.',[sSysInfo.dwPageSize]));
        dwPageSize := sSysInfo.dwPageSize;
        lpvAddr := VirtualAlloc(p, dwPageSize, MEM_RESERVE or MEM_COMMIT, PAGE_READONLY or PAGE_GUARD);
        p := pointer($3600000);
        lpvAddr := VirtualAlloc(p, cSpaceMin, MEM_RESERVE, PAGE_NOACCESS);

        if(lpvAddr = nil) 
          then add(format('VirtualAlloc failed. Error: %d', [GetLastError]))
          else
            begin
              add(format('Committed %d bytes at address %x', [dwPageSize, integer(lpvAddr)]));
              bLocked := VirtualLock(lpvAddr, dwPageSize);
              if bLocked
                then  add(format('Lock Achieved at %x', [integer(lpvAddr)]))
                else add(format('Cannot lock at %x, error = %d', [integer(lpvAddr), GetLastError]));
              bLocked := VirtualLock(lpvAddr, dwPageSize);
              if bLocked 
                then add(format('2nd Lock Achieved at %x', [integer(lpvAddr)]))
                else add(format('Cannot get 2nd lock at %x, error = %d', [integer(lpvAddr), GetLastError]));
            end;
        end;
    end;

procedure TForm2.ToolButton4Click(Sender: TObject);
  begin
    SavePers;
  end;

end.

      
