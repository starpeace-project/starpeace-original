unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, ImgList, ToolWin;

type
  TForm1 = 
    class(TForm)
        TreeView1: TTreeView;
        ToolBar1: TToolBar;
        StatusBar1: TStatusBar;
        ToolButton1: TToolButton;
        ToolButton2: TToolButton;
        ImageList1: TImageList;
        RichEdit1: TRichEdit;
        Splitter1: TSplitter;
        ToolButton3: TToolButton;
        ToolButton4: TToolButton;
        procedure ToolButton2Click(Sender: TObject);
        procedure ToolButton1Click(Sender: TObject);
        procedure ToolButton4Click(Sender: TObject);
      private
        { Private declarations }
      public
        { Public declarations }
    end;

var
  Form1: TForm1;

implementation

{$R *.DFM}
uses
  MemDll_Interfaz;
    
  procedure CreateObj; external 'DllPersistent.dll';
  procedure SavePers; external 'DllPersistent.dll';
  function GetMemmory(const i: integer):pointer; external 'DllPersistent.dll';

function IsObject(a: pointer; Size: integer): boolean;
  var
    AObject: TObject;
    AClass: TClass;
  type
    PPointer = ^Pointer;
  begin
    try
      if Size >= 4 
        then 
          begin
            AObject := TObject(a);
            AClass := AObject.ClassType;
            result := (Integer(AClass) >= 64*1024) and (PPointer(PChar(AClass) + vmtSelfPtr)^ = Pointer(AClass));
          end
        else result := false;
    except
      result := false
    end;
  end;
  
procedure TForm1.ToolButton2Click(Sender: TObject);
  begin
    CreateObj;
  end;

procedure TForm1.ToolButton1Click(Sender: TObject);
  var
    vNode : TTreeNode;
  procedure AddBlockDesc(BlockDesc: PBlockDesc);
    var
      b    : PBlockDesc;
      tmpNode : TTreeNode;
    begin
      b := BlockDesc.next;
      if b<>nil
        then
          begin
            tmpNode := vNode;
            while b <> BlockDesc do 
              begin
                tmpNode := TreeView1.Items.AddChildObject(tmpNode, format('Addr %x, (%d)', [integer(b.addr), b.Size]), b);
                b := b.next;
              end;
          end;
    end;

  procedure AddFree(aFree: PFree);
    var
      f : PFree;
      tmpNode : TTreeNode;
    begin
      if aFree<>nil
        then
          begin
            f := aFree.next;
            tmpNode := TreeView1.Items.AddChild(vNode, format('Free %x', [integer(f)]));
            if f <> nil
              then
                while (f <> aFree) do 
                  begin
                    if (f.prev.next <> f) or (f.next.prev <> f) or (f.size < sizeof(TFree)) 
                      then 
                        begin
                          TreeView1.Items.AddChild(tmpNode, 'Bad Free List');
                          break;
                        end;
                    tmpNode := TreeView1.Items.AddChildObject(tmpNode, format('Free %d', [f.Size]), f);
                    f := f.next;
                  end;
          end;
    end;

  procedure AddSmallTab;
    var
      smallTab : PSmallTab;       
      i        : integer;
      f        : PFree; 
      tmpNode  : TTreeNode;
    begin
      smallTab := Get_smallTab; 
      if smallTab<>nil
        then
          for i := Low(smallTab^) to High(smallTab^) do 
            begin
              f := smallTab[i];
              if f <> nil then 
                begin
                  tmpNode := TreeView1.Items.AddChild(vNode, format('smallTab (%x): %d',[integer(f),i]));
                  repeat
                    TreeView1.Items.AddChild(tmpNode, format(' (%x): %d',[integer(f), f.size]));
                    if (f.prev.next <> f) or (f.next.prev <> f) or (f.size < sizeof(TFree)) 
                      then 
                        begin
                          TreeView1.Items.AddChild(tmpNode, 'Bad Free List');
                          break;
                        end;
                    f := f.next;
                  until f = smallTab[i];
                end;
            end;    
    end;
    
  procedure Committed;
    var
      a, e: PChar;
      b: PBlockDesc;
      f : PFree;
      prevFree: Boolean;
      committedRoot: PBlockDesc; 
      curAlloc : pchar;        
      remBytes : integer;
      size  : integer;
      freeSize : integer;
      tmpNode  : TTreeNode;
    begin
      committedRoot := GetcommittedRoot;
      curAlloc := Get_curAlloc;
      remBytes := Get_remBytes;
      b := committedRoot.next;
      FreeSize := 0;
      prevFree := False;
      if (b<>nil) 
        then 
          begin
            vNode := TreeView1.Items.AddChild(vNode, 'MemUsed');
            while b <> committedRoot do 
              begin
                a := b.addr;
                e := a + b.size;
                tmpNode := TreeView1.Items.AddChild(vNode, format('Addr(%x) Size(%d)', [integer(a), b.size]));
                while a < e do 
                  begin
                    if (a = curAlloc) and (remBytes > 0) 
                      then 
                        begin
                          size := remBytes;
                          Inc(freeSize, size);
                          if prevFree 
                            then tmpNode := TreeView1.Items.AddChild(tmpNode, 'Bad Cur Alloc');
                          TreeView1.Items.AddChild(tmpNode, format('freeSize:remBytes (%xh):%d', [integer(a),freeSize]));
                          prevFree := False;
                        end 
                      else 
                        begin
                          if prevFree <> ((PUsed(a).sizeFlags and cPrevFreeFlag) <> 0) 
                            then TreeView1.Items.AddChild(tmpNode, 'Bad Cur Alloc');
                          if (PUsed(a).sizeFlags and cThisUsedFlag) = 0 
                            then 
                              begin
                                f := PFree(a);
                                if (f.prev.next <> f) or (f.next.prev <> f) or (f.size < sizeof(TFree)) 
                                  then tmpNode := TreeView1.Items.AddChild(tmpNode, 'Bad Free Block');
                                TreeView1.Items.AddChild(tmpNode, format('free (%xh):%d', [integer(f), f.size]));
                                size := f.size;
                                Inc(freeSize, size);
                                prevFree := True;
                              end 
                            else 
                              begin
                                size := PUsed(a).sizeFlags and not cFlags;
                                if (PUsed(a).sizeFlags and cFillerFlag) <> 0 
                                  then 
                                    begin
      //                                Inc(result.overhead, size);
                                      if (a > b.addr) and (a + size < e) 
                                        then tmpNode := TreeView1.Items.AddChild(tmpNode, 'Bad Used Block');
                                      TreeView1.Items.AddChild(tmpNode, format('OverHead (%xh):%d', [integer(a), size]));
                                    end 
                                  else 
                                    begin
                                      if IsObject(a+sizeof(TUsed),size-sizeof(TUsed))
                                        then TreeView1.Items.AddChild(tmpNode, format('user (%xh):%d '+tobject(a+sizeof(TUsed)).classname, [integer(a+sizeof(TUsed)),size-sizeof(TUsed)]))
                                        else TreeView1.Items.AddChild(tmpNode, format('user (%xh):%d ', [integer(a+sizeof(TUsed)),size-sizeof(TUsed)]));
                                      TreeView1.Items.AddChild(tmpNode, format('overhead (%xh):%d', [integer(a), sizeof(TUsed)]));
      //                                Inc(userSize, size-sizeof(TUsed));
      //                                Inc(result.overhead, sizeof(TUsed));
                                    end;
                                prevFree := False;
                              end;
                        end;
                    Inc(a, size);
                  end;
                b := b.next;
              end;
          end;
    end;
  begin
    with TreeView1.Items do
      begin
        BeginUpdate;
        clear;
        vNode := Add(nil, 'spaceRoot');
        AddBlockDesc(GetspaceRoot);
        
        vNode := Add(nil, 'decommittedRoot');
        AddBlockDesc(GetdecommittedRoot);

        vNode := Add(nil, 'GetcommittedRoot');
        AddBlockDesc(GetcommittedRoot);
        
        vNode := Add(nil, 'AddSmall');
        AddSmallTab;
        
        vNode := Add(nil, 'avail');
        AddFree(Get_avail);
        
        vNode := Add(nil, 'avail');
        AddFree(Get_rover);

        vNode := Add(nil, format('remBytes (%d)',[Get_remBytes]));
        vNode := Add(nil, format('curAlloc (%x)',[integer(Get_curAlloc)]));

        Committed;        
        EndUpdate;
      end;
  end;


procedure TForm1.ToolButton4Click(Sender: TObject);
  begin
    GetMemmory(10000);
  end;

end.
 