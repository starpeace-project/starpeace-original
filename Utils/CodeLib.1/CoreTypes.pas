unit CoreTypes;

interface

uses
  Classes;

type
  IEnumStrings =
    interface
      function  Next(out which : array of string) : integer;
      function  Skip(count : integer) : integer;
      procedure Reset;
    end;

type
  IEnumClasses =
    interface
      function  Next(out which : array of TClass) : integer;
      function  Skip(count : integer) : integer;
      procedure Reset;
    end;

type
  IEnumObjects =
    interface
      function  Next(out which : array of TObject) : integer;
      function  Skip(count : integer) : integer;
      procedure Reset;
    end;

type
  ICollection =
    interface
      function  GetCount : integer;
      procedure SetCount(NewCount : integer);
      function  Get(Index : integer) : pointer;
      procedure Put(Index : integer; Item : pointer);
      function  Add(Item : pointer) : integer;
      procedure Clear;
      procedure Delete(Index : integer);
      function  IndexOf(Item : pointer) : integer;
      procedure Insert(Index : integer; Item : pointer);
      function  Remove(Item : pointer) : integer;
      procedure Pack;
      function  List : PPointerList;
      property Count : integer read GetCount write SetCount;
      property Items[idx : integer] : pointer read Get write Put; default;
    end;

type
  TRelationshipKind = (rkUse, rkContaining);

const
  rkBelonguer = rkContaining;

type
  TSyncronizedList =
    class
      public
        constructor Create(aRelKind : TRelationshipKind);
      public
        function Lock : ICollection;
      private
        fItems : ICollection;
    end;


implementation


uses
  Windows;

type
  TLockableList =
    class(Tlist, ICollection)
      public
        constructor Create(aRelKind : TRelationshipKind);
        destructor  Destroy;   override;
      private // IUnknown
        fRelKind  : TRelationshipKind;
        fRefCount : integer;
        fLock     : TRTLCriticalSection;
        function QueryInterface(const iid : TGUID; out obj) : hresult;   stdcall;
        function _AddRef : integer;   stdcall;
        function _Release : integer;   stdcall;
      private // ICollection
        function  ICollection.List = GetList;
        function  GetCount : integer;
        function  GetList : PPointerList;
      private
        procedure Lock;
        procedure Unlock;
    end;

// TSyncronizedList

constructor TSyncronizedList.Create(aRelKind : TRelationshipKind);
  begin
    inherited Create;
    fItems := TLockableList.Create(aRelKind);
  end;

function TSyncronizedList.Lock : ICollection;
  begin
    Result := fItems;
  end;


// TLockableList

constructor TLockableList.Create(aRelKind : TRelationshipKind);
  begin
    inherited Create;
    fRelKind := aRelKind;
    InitializeCriticalSection(fLock);
  end;

destructor TLockableList.Destroy;
  var
    i  : integer;
    pp : PPointerList;
  begin
    if fRelKind = rkContaining
      then
        begin
          pp := List;
          for i := pred(Count) downto 0 do
            TObject(pp[i]).Free;
        end;
    DeleteCriticalSection(fLock);
    inherited;
  end;

function TLockableList.QueryInterface(const iid : TGUID; out obj) : hresult;
  const
    E_NOINTERFACE = $80004002;
  begin
    if GetInterface(iid, obj)
      then Result := 0
      else Result := E_NOINTERFACE;
  end;

function TLockableList._AddRef : integer;
  begin
    if fRefCount = 1
      then Lock;
    inc(fRefCount);
    Result := fRefCount;
  end;

function TLockableList._Release : integer;
  begin
    dec(fRefCount);
    Result := fRefCount;
    if fRefCount = 1
      then Unlock
      else
        if fRefCount = 0
          then Destroy;
  end;


function TLockableList.GetCount : integer;
  begin
    Result := Count;
  end;

function TLockableList.GetList : PPointerList;
  begin
    Result := List;
  end;

procedure TLockableList.Lock;
  begin
    EnterCriticalSection(fLock);
  end;

procedure TLockableList.Unlock;
  begin
    LeaveCriticalSection(fLock);
  end;


end.

