unit Containers;

interface

  uses
    SysUtils, Classes, HashTables;

  const
    DEFAULTBUFSIZE = 1024;

  type
    // Forward declarations
    TItemReader = class;
    TItemWriter = class;

    // The item type
    TItem = class
    protected
      constructor Read(Reader: TItemReader); virtual;
      procedure Write(Writer: TItemWriter); virtual;
    public
      procedure Associate(AObject: TObject); virtual;
      procedure Dissociate(AObject: TObject); virtual;
    end;
    TItemClass = class of TItem;

    // Abstract base class for all types of containers
    TContainer = class(TItem)
    private
      FHost: TItem;
      FOwnsItems: Boolean;
    protected
      constructor Read(Reader: TItemReader); override;
      procedure Write(Writer: TItemWriter); override;
      function GetCount: Integer; virtual; abstract;
    public
      constructor Create(AHost: TItem; AOwnsItems: Boolean = false); virtual;
      destructor Destroy; override;
      procedure Assign(Source: TContainer); virtual;
      procedure Clear; virtual; abstract;
      function First: TItem; virtual; abstract;
      function Next: TItem; virtual; abstract;
      property Count: Integer read GetCount;
      property Host: TItem read FHost;
      property OwnsItems: Boolean read FOwnsItems write FOwnsItems;
    end;

    // Ordered, random access list of items
    TSequence = class(TContainer)
    private
      FList: TList;
      FIndex: Integer;
    protected
      constructor Read(Reader: TItemReader); override;
      procedure Write(Writer: TItemWriter); override;
      function GetCount: Integer; override;
      function GetItems(Index: Integer): TItem;
    public
      constructor Create(AHost: TItem; AOwnsItems: Boolean = false); override;
      destructor Destroy; override;
      procedure Assign(Source: TContainer); override;
      function Add(Item: TItem): Integer;
      procedure Clear; override;
      procedure Delete(Index: Integer);
      procedure Exchange(Index1, Index2: Integer);
      function First: TItem; override;
      function Next: TItem; override;
      function IndexOf(Item: TItem): Integer;
      procedure Insert(Index: Integer; Item: TItem);
      procedure Move(CurIndex, NewIndex: Integer);
      function Remove(Item: TItem): Integer;
      property Items[Index: Integer]: TItem read GetItems; default;
    end;

    // Generic set container
    TSet = class(TContainer)
    private
      FTable: THashTable;
    protected
      constructor Read(Reader: TItemReader); override;
      procedure Write(Writer: TItemWriter); override;
      function GetCount: Integer; override;
    public
      constructor Create(AHost: TItem; AOwnsItems: Boolean = false); override;
      destructor Destroy; override;
      procedure Assign(Source: TContainer); override;
      procedure Clear; override;
      function First: TItem; override;
      function Next: TItem; override;
      function Belongs(Item: TItem): Boolean;
      procedure Insert(Item: TItem);
      procedure Intersection(Other: TSet);
      function Remove(Item: TItem): Boolean;
      procedure Subtract(Other: TSet);
      procedure Union(Other: TSet);
    end;

    // Dictionary with string key as a key type
    TDictionary = class(TContainer)
    private
      FTable: TStringTable;
    protected
      constructor Read(Reader: TItemReader); override;
      procedure Write(Writer: TItemWriter); override;
      function GetCount: Integer; override;
      function GetItems(const Key: string): TItem;
    public
      constructor Create(AHost: TItem; AOwnsItems: Boolean = false); override;
      destructor Destroy; override;
      procedure Assign(Source: TDictionary); reintroduce;
      procedure Clear; override;
      function First: TItem; override;
      function Next: TItem; override;
      procedure Insert(const Key: string; Item: TItem);
      function Remove(const Key: string): TItem;
      property Items[const Key: string]: TItem read GetItems; default;
    end;

    // Call back procedure that is called by the TItemReader when
    // a reference is resolved
    TResolveCallBack = procedure(Item: TItem) of object;

    // Node corresponding to a call back reference
    TCallBackNode = record
      CallBack: TResolveCallBack;
      Reference: Integer;
    end;
    PCallBackNode = ^TCallBackNode;

    // TItemReader class extends Delphi's TReader class to handle object
    // serialization
    TItemReader = class(TReader)
    private
      FClasses: THashTable;
      FItems: THashTable;
      FReferences: TList;
      FCallBackRefs: TList;
    public
      constructor Create(Stream: TStream);
      destructor Destroy; override;
      procedure ReadClassTable;
      function ReadItem: TItem;
      procedure ReadReference(var AReference: TItem);
      procedure ReadRefCallBack(CallBack: TResolveCallBack);
      procedure ResolveReferences;
    end;

    TItemWriter = class(TWriter)
    public
      constructor Create(Stream: TStream);
      procedure WriteClassTable;
      procedure WriteItem(AItem: TItem);
      procedure WriteReference(AReference: TItem);
    end;

    EContainerError = class(Exception);

  procedure RegisterItemClass(AClass: TItemClass);
  procedure RegisterItemClasses(AClasses: array of TItemClass);
  function FindItemClass(const AClassName: string): TItemClass;

  function LoadItemFromStream(Stream: TStream): TItem;
  procedure SaveItemToStream(Item: TItem; Stream: TStream);
  function LoadItemFromFile(const FileName: string): TItem;
  procedure SaveItemToFile(Item: TItem; const FileName: string);

implementation

  var
    ItemClassList: TList;

  // General object management

  procedure ContainerError(const Msg: string);
    begin
      raise EContainerError.Create(Msg);
    end;

  procedure RegisterItemClass(AClass: TItemClass);
    begin
      if ItemClassList.IndexOf(AClass) >= 0
        then
          ContainerError(Format('Class %s is already registered', [AClass.ClassName]));
      ItemClassList.Add(AClass);
    end;

  procedure RegisterItemClasses(AClasses: array of TItemClass);
    var
      i: Integer;
    begin
      for i := Low(AClasses) to High(AClasses) do
        RegisterItemClass(AClasses[i]);
    end;

  function FindItemClass(const AClassName: string): TItemClass;
    var
      i: Integer;
    begin
      Result := nil;
      i := ItemClassList.Count - 1;
      while (i >= 0) and (TItemClass(ItemClassList[i]).ClassName <> AClassName) do
        Dec(i);
      if i >= 0
        then Result := TItemClass(ItemClassList[i])
        else ContainerError(Format('Class %s is not registered', [AClassName]));
    end;

  // Serialization routines

  function LoadItemFromStream(Stream: TStream): TItem;
    var
      Reader: TItemReader;
    begin
      Reader := TItemReader.Create(Stream);
      try
        Reader.ReadClassTable;
        Result := Reader.ReadItem;
        Reader.ResolveReferences;
      finally
        Reader.Free;
      end;
    end;

  procedure SaveItemToStream(Item: TItem; Stream: TStream);
    var
      Writer: TItemWriter;
    begin
      Writer := TItemWriter.Create(Stream);
      try
        Writer.WriteClassTable;
        Writer.WriteItem(Item);
      finally
        Writer.Free;
      end;
    end;

  function LoadItemFromFile(const FileName: string): TItem;
    var
      Stream: TFileStream;
    begin
      Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
      try
        Result := LoadItemFromStream(Stream);
      finally
        Stream.Free;
      end;
    end;

  procedure SaveItemToFile(Item: TItem; const FileName: string);
    var
      Stream: TFileStream;
    begin
      Stream := TFileStream.Create(FileName, fmCreate);
      try
        SaveItemToStream(Item, Stream);
      finally
        Stream.Free;
      end;
    end;


  { TItem }

  procedure TItem.Associate(AObject: TObject);
    begin
      // Empty declaration
    end;

  procedure TItem.Dissociate(AObject: TObject);
    begin
      // Empty declaration
    end;

  constructor TItem.Read(Reader: TItemReader);
    begin
      // Empty declaration
    end;

  procedure TItem.Write(Writer: TItemWriter);
    begin
      // Empty declaration
    end;

  { TContainer }

  procedure TContainer.Assign(Source: TContainer);
    begin
      //Empty declaration
    end;

  constructor TContainer.Create(AHost: TItem; AOwnsItems: Boolean);
    begin
      FOwnsItems := AOwnsItems;
      FHost := AHost;
    end;

  destructor TContainer.Destroy;
    var
      it: TItem;
    begin
      it := First;
      while it <> nil do
      begin
        it.Dissociate(self);
        it := Next;
      end;
      Clear;
    end;

  constructor TContainer.Read(Reader: TItemReader);
    begin
      with Reader do
      begin
        ReadReference(FHost);
        FOwnsItems := ReadBoolean;
      end;
    end;

  procedure TContainer.Write(Writer: TItemWriter);
    begin
      with Writer do
      begin
        WriteReference(FHost);
        WriteBoolean(FOwnsItems);
      end;
    end;

  { TSequence }

  function TSequence.Add(Item: TItem): Integer;
    begin
      Result := FList.Add(Item);
      Item.Associate(self);
    end;

  procedure TSequence.Assign(Source: TContainer);
    var
      it: TItem;
    begin
      Clear;
      it := Source.First;
      while it <> nil do
      begin
        FList.Add(it);
        it := Source.Next;
      end;
    end;

  procedure TSequence.Clear;
    var
      i: Integer;
    begin
      if OwnsItems then
        for i := 0 to FList.Count - 1 do
          TItem(FList[i]).Free;
      FList.Clear;
    end;

  constructor TSequence.Create(AHost: TItem; AOwnsItems: Boolean);
    begin
      inherited Create(AHost, AOwnsItems);
      FList := TList.Create;
    end;

  procedure TSequence.Delete(Index: Integer);
    begin
      if OwnsItems then
        TItem(FList[Index]).Free
      else TItem(FList[Index]).Dissociate(self);
      FList.Delete(Index);
    end;

  destructor TSequence.Destroy;
    begin
      inherited Destroy;
      FList.Free;
    end;

  procedure TSequence.Exchange(Index1, Index2: Integer);
    begin
      FList.Exchange(Index1, Index2);
    end;

  function TSequence.First: TItem;
    begin
      FIndex := 0;
      if FList.Count > 0 then
        Result := TItem(FList.First)
      else Result := nil;
    end;

  function TSequence.GetCount: Integer;
    begin
      Result := FList.Count;
    end;

  function TSequence.GetItems(Index: Integer): TItem;
    begin
      Result := TItem(FList[Index]);
    end;

  function TSequence.IndexOf(Item: TItem): Integer;
    begin
      Result := FList.IndexOf(Item);
    end;

  procedure TSequence.Insert(Index: Integer; Item: TItem);
    begin
      FList.Insert(Index, Item);
      Item.Associate(self);
    end;

  procedure TSequence.Move(CurIndex, NewIndex: Integer);
    begin
      FList.Move(CurIndex, NewIndex);
    end;

  function TSequence.Next: TItem;
    begin
      if FIndex < (FList.Count - 1) then
      begin
        Inc(FIndex);
        Result := FList[FIndex];
      end
      else Result := nil;
    end;

  constructor TSequence.Read(Reader: TItemReader);
    begin
      inherited Read(Reader);
      FList := TList.Create;
      with Reader do
      begin
        FList.Capacity := ReadInteger;
        ReadListBegin;
        while not EndOfList do
          if OwnsItems then Add(ReadItem)
          else begin
            FList.Add(nil);
            ReadReference(TItem(FList.List[FList.Count - 1]));
          end;
        ReadListEnd;
      end;
    end;

  function TSequence.Remove(Item: TItem): Integer;
    begin
      Result := FList.Remove(Item);
      if OwnsItems then
        Item.Free
      else Item.Dissociate(self);
    end;


  procedure TSequence.Write(Writer: TItemWriter);
    var
      i: Integer;
    begin
      inherited Write(Writer);
      with Writer do
      begin
        WriteInteger(Count);
        WriteListBegin;
        for i := 0 to FList.Count - 1 do
          if OwnsItems then WriteItem(Items[i])
          else WriteReference(Items[i]);
        WriteListEnd;
      end;
    end;

  { TDictionary }

  procedure TDictionary.Assign(Source: TDictionary);
    var
      pt: Pointer;
    begin
      Clear;
      pt := (Source as TDictionary).FTable.First;
      while pt <> nil do
      begin
        FTable.Insert((Source as TDictionary).FTable.CurrentKey, pt);
        pt := (Source as TDictionary).FTable.Next;
      end;
    end;

  procedure TDictionary.Clear;
    var
      it: TItem;
    begin
      if OwnsItems then
      begin
        it := FTable.First as TItem;
        while it <> nil do
        begin
          it.Free;
          it := FTable.Next as TItem;
        end;
      end;
      FTable.Clear;
    end;

  constructor TDictionary.Create(AHost: TItem; AOwnsItems: Boolean);
    begin
      inherited Create(AHost, AOwnsItems);
      FTable := TStringTable.Create;
    end;

  destructor TDictionary.Destroy;
    begin
      inherited Destroy;
      FTable.Free;
    end;

  function TDictionary.First: TItem;
    begin
      Result := FTable.First as TItem;
    end;

  function TDictionary.GetCount: Integer;
    begin
      Result := FTable.Count;
    end;

  function TDictionary.GetItems(const Key: string): TItem;
    begin
      Result := FTable[Key] as TItem;
    end;

  procedure TDictionary.Insert(const Key: string; Item: TItem);
    begin
      FTable.Insert(Key, Item);
      Item.Associate(self);
    end;

  function TDictionary.Next: TItem;
    begin
      Result := FTable.Next as TItem;
    end;

  constructor TDictionary.Read(Reader: TItemReader);
    var
      key: string;
    begin
      inherited Read(Reader);
      FTable := TStringTable.Create;
      with Reader do
      begin
        FTable.Capacity := ReadInteger;
        ReadListBegin;
        while not EndOfList do
        begin
          key := ReadString;
          if OwnsItems then Insert(key, ReadItem)
          else ReadReference(TItem(FTable.Insert(key, nil)^));
        end;
        ReadListEnd;
      end;
    end;

  function TDictionary.Remove(const Key: string): TItem;
    begin
      Result := FTable.Remove(Key) as TItem;
      if Result <> nil then
      begin
        if OwnsItems then Result.Free
        else Result.Dissociate(self);
      end;
    end;

  procedure TDictionary.Write(Writer: TItemWriter);
    var
      it: TItem;
    begin
      inherited Write(Writer);
      with Writer do
      begin
        WriteInteger(Count);
        WriteListBegin;
        it := First;
        while it <> nil do
        begin
          WriteString(FTable.CurrentKey);
          if OwnsItems then WriteItem(it)
          else WriteReference(it);
          it := Next;
        end;
        WriteListEnd;
      end;
    end;

  { TSet }

  procedure TSet.Assign(Source: TContainer);
    var
      it: TItem;
    begin
      Clear;
      it := Source.First;
      while it <> nil do
      begin
        FTable.Insert(Integer(it), it);
        it := Source.Next;
      end;
    end;

  function TSet.Belongs(Item: TItem): Boolean;
    begin
      Result := (FTable[Integer(Item)] <> nil);
    end;

  procedure TSet.Clear;
    var
      it: TItem;
    begin
      if OwnsItems then
      begin
        it := TItem(FTable.First);
        while it <> nil do
        begin
          it.Free;
          it := TItem(FTable.Next);
        end;
      end;
      FTable.Clear;
    end;

  constructor TSet.Create(AHost: TItem; AOwnsItems: Boolean);
    begin
      inherited Create(AHost, AOwnsItems);
      FTable := THashTable.Create;
    end;

  destructor TSet.Destroy;
    begin
      inherited Destroy;
      FTable.Free;
    end;

  function TSet.First: TItem;
    begin
      Result := TItem(FTable.First);
    end;

  function TSet.GetCount: Integer;
    begin
      Result := FTable.Count;
    end;

  procedure TSet.Insert(Item: TItem);
    begin
      FTable[Integer(Item)] := Item;
      Item.Associate(self);
    end;

  procedure TSet.Intersection(Other: TSet);
    var
      pt: Pointer;
    begin
      pt := FTable.First;
      while pt <> nil do
      begin
        if Other.FTable[Integer(pt)] = nil then
          FTable.DeleteCurrent;
        pt := FTable.Next;
      end;
    end;

  function TSet.Next: TItem;
    begin
      Result := TItem(FTable.Next);
    end;

  constructor TSet.Read(Reader: TItemReader);
    begin
      inherited Read(Reader);
      FTable := THashTable.Create;
      with Reader do
      begin
        FTable.Capacity := ReadInteger;
        ReadListBegin;
        while not EndOfList do
        begin
          if OwnsItems then Insert(ReadItem)
          else ReadRefCallBack(Insert);
        end;
        ReadListEnd;
      end;
    end;

  function TSet.Remove(Item: TItem): Boolean;
    begin
      Result := (FTable.Remove(Integer(Item)) <> nil);
      if Result then
      begin
        if OwnsItems then Item.Free
        else Item.Dissociate(self);
      end;
    end;

  procedure TSet.Subtract(Other: TSet);
    var
      pt: Pointer;
    begin
      pt := FTable.First;
      while pt <> nil do
      begin
        if Other.FTable[Integer(pt)] <> nil then
          FTable.DeleteCurrent;
        pt := FTable.Next;
      end;
    end;

  procedure TSet.Union(Other: TSet);
    var
      pt: Pointer;
    begin
      pt := Other.FTable.First;
      while pt <> nil do
      begin
        if FTable[Integer(pt)] = nil then
          FTable.Insert(Integer(pt), pt);
        pt := Other.FTable.Next;
      end;
    end;

  procedure TSet.Write(Writer: TItemWriter);
    var
      it: TItem;
    begin
      inherited Write(Writer);
      with Writer do
      begin
        WriteInteger(Count);
        WriteListBegin;
        it := First;
        while it <> nil do
        begin
          if OwnsItems then WriteItem(it)
          else WriteReference(it);
          it := Next;
        end;
        WriteListEnd;
      end;
    end;

  { TItemReader }

  constructor TItemReader.Create(Stream: TStream);
    begin
      inherited Create(Stream, DEFAULTBUFSIZE);
      FClasses := THashTable.Create;
      FItems := THashTable.Create;
      FReferences := TList.Create;
      FCallBackRefs := TList.Create;
    end;

  destructor TItemReader.Destroy;
    var
      i: Integer;
    begin
      FClasses.Free;
      FItems.Free;
      FReferences.Free;
      for i := 0 to FCallBackRefs.Count - 1 do
        if FCallBackRefs[i] <> nil then
          Dispose(PCallBackNode(FCallBackRefs[i]));
      FCallBackRefs.Free;
      inherited Destroy;
    end;

  procedure TItemReader.ReadClassTable;
    var
      key: Integer;
    begin
      ReadListBegin;
      while not EndOfList do
      begin
        key := ReadInteger;
        FClasses.Insert(key, FindItemClass(ReadString));
      end;
      ReadListEnd;
    end;

  function TItemReader.ReadItem: TItem;
    var
      ic: TItemClass;
      key: Integer;
    begin
      ic := TItemClass(FClasses[ReadInteger]);
      key := ReadInteger;
      Result := ic.Read(self);
      FItems.Insert(key, Result);
    end;

  procedure TItemReader.ReadRefCallBack(CallBack: TResolveCallBack);
    var
      cbn: PCallBackNode;
    begin
      New(cbn);
      cbn^.CallBack := CallBack;
      cbn^.Reference := ReadInteger;
      FCallBackRefs.Add(cbn);
    end;

  procedure TItemReader.ReadReference(var AReference: TItem);
    var
      pt: Pointer;
    begin
      pt := Pointer(ReadInteger);
      if pt = nil then AReference := nil
      else begin
        FReferences.Add(@AReference);
        FReferences.Add(pt);
      end;
    end;

  procedure TItemReader.ResolveReferences;
    var
      i: Integer;
      it: TItem;
      cbn: PCallBackNode;
    begin
      i := 0;
      while i < FReferences.Count do
      begin
        it := TItem(FItems[Integer(FReferences[i + 1])]);
        if it = nil then
          ContainerError('Could not resolve references');
        TItem(FReferences[i]^) := it;
        Inc(i, 2);
      end;
      FReferences.Clear;
      for i := 0 to FCallBackRefs.Count - 1 do
      begin
        cbn := FCallBackRefs[i];
        if cbn^.Reference = 0 then cbn^.CallBack(nil)
        else begin
          it := TItem(FItems[cbn^.Reference]);
          if it = nil then
            ContainerError('Could not resolve references');
          cbn^.CallBack(it);
        end;
        Dispose(cbn);
        FCallBackRefs[i] := nil;
      end;
      FCallBackRefs.Clear;
      FItems.Clear;
      FClasses.Clear;
    end;

  { TItemWriter }

  constructor TItemWriter.Create(Stream: TStream);
    begin
      inherited Create(Stream, DEFAULTBUFSIZE);
    end;

  procedure TItemWriter.WriteClassTable;
    var
      i: Integer;
    begin
      WriteListBegin;
      for i := 0 to ItemClassList.Count - 1 do
      begin
        WriteInteger(Integer(ItemClassList[i]));
        WriteString(TItemClass(ItemClassList[i]).ClassName);
      end;
      WriteListEnd;
    end;

  procedure TItemWriter.WriteItem(AItem: TItem);
    begin
      if ItemClassList.IndexOf(AItem.ClassType) < 0 then
        ContainerError(Format('Class %s is not registered', [AItem.ClassName]));
      WriteInteger(Integer(AItem.ClassType));
      WriteInteger(Integer(AItem));
      AItem.Write(self);
    end;

  procedure TItemWriter.WriteReference(AReference: TItem);
    begin
      WriteInteger(Integer(AReference));
    end;


initialization
  ItemClassList := TList.Create;
  RegisterItemClasses([TSequence, TSet, TDictionary]);

finalization
  ItemClassList.Free;

end.
