unit InternationalizerComponent;

interface

  uses
    Windows, SysUtils, Classes;

  type
    TInternationalizerComponent =
      class(TComponent)
        public
          constructor Create(Owner : TComponent); override;
          destructor  Destroy; override;
        private
          fTransFilesPath : string;
          fLanguageId     : integer;
          fModifyForm     : boolean;
          fReadAll        : boolean;
          procedure SetTransFilesPath(const path : string);
          procedure SetLanguageId(languageid : integer);
          procedure SetModifyForm(modifyform : boolean);
        private
          fPropertyNames : TStringList;
        protected
          procedure Loaded; override;
        public
          procedure GenerateLanguageFile;
          procedure ModifyProperties;
        published
          property TransFilesPath : string  read fTransFilesPath write SetTransFilesPath;
          property ModifyForm     : boolean read fModifyForm     write SetModifyForm;
          property LanguageId     : integer read fLanguageId     write SetLanguageId;
          property ReadAll        : boolean read fReadAll        write fReadAll;
      end;

  procedure Register;

  procedure SetBasePath(const transfilespath : string);
  procedure SetLanguage(languageid : integer);

implementation

  uses
    TypInfo, Forms, ComCtrls, VisualControls;

  const
    cDefaultLanguageId = 0;

  const
    cValidPropTypes  = [tkString, tkLString, tkClass];
    cSimplePropTypes = [tkString, tkLString];

  const
    cDefPropertyCount = 7;

  const
    cDefaultProperties : array [1..cDefPropertyCount] of string =
      (
        'Text',
        'Caption',
        'Hint',
        'Items',
        'Lines',
        'TabNames',
        'Columns'
      );

  // Utils

  procedure AssignObject(var Dest : TObject; const Source : TObject);
    begin
      Dest := Source;
    end;

  procedure GetObjectProp(Instance: TObject; PropInfo: PPropInfo; var Value : TObject); assembler;
    asm
        { ->    EAX Pointer to instance         }
        {       EDX Pointer to property info    }
        {       ECX Pointer to result object    }

        push    esi
        push    edi
        mov     edi, edx

        mov     edx, [edi].TPropInfo.Index       { pass index in EDX }
        cmp     edx, $80000000
        jne     @@hasIndex
        mov     edx, ecx                         { pass value in EDX }
      @@hasIndex:
        mov     esi, [edi].TPropInfo.GetProc
        cmp     [edi].TPropInfo.GetProc.Byte[3],$FE
        ja      @@isField
        jb      @@isStaticMethod

      @@isVirtualMethod:
        movsx   esi, si                          { sign extend slot offset }
        add     esi, [eax]                       { vmt + slot offset }
        call    DWORD PTR [esi]
        jmp     @@exit

      @@isStaticMethod:
        call    esi
        jmp     @@exit

      @@isField:
        and	esi, $00FFFFFF
        mov	edx, [eax + esi]
        mov	eax, ecx
        call	AssignObject

      @@exit:
        pop   edi
        pop   esi
    end;

  var
    vInternationalizerComps : TList;
    vTransFilesPath         : string = '';
    vLanguageId             : integer = 0;

  procedure RegisterInternationalizer(Internationalizer : TInternationalizerComponent); forward;

  // TInternationalizerComponent

  constructor TInternationalizerComponent.Create(Owner : TComponent);
    var
      i : integer;
    begin
      inherited;
      fPropertyNames := TStringList.Create;
      for i := 1 to cDefPropertyCount do
        fPropertyNames.Add(cDefaultProperties[i]);
      fLanguageId := cDefaultLanguageId;
      if csDesigning in ComponentState
        then
          if fModifyForm
            then ModifyProperties
            else GenerateLanguageFile;
      if not (csDesigning in ComponentState)
        then RegisterInternationalizer(Self);
    end;

  destructor TInternationalizerComponent.Destroy;
    begin
      fPropertyNames.Free;
      inherited;
    end;

  procedure TInternationalizerComponent.SetTransFilesPath(const path : string);
    begin
      fTransFilesPath := path;
      if (fTransFilesPath <> '') and (fTransFilesPath[length(fTransFilesPath)] <> '\')
        then fTransFilesPath := fTransFilesPath + '\';
    end;

  procedure TInternationalizerComponent.SetLanguageId(languageid : integer);
    begin
      fLanguageId := languageid;
      if fModifyForm
        then ModifyProperties
        else
          if csDesigning in ComponentState
            then GenerateLanguageFile;
    end;

  procedure TInternationalizerComponent.SetModifyForm(modifyform : boolean);
    begin
      if fModifyForm <> modifyform
        then
          begin
            fModifyForm := modifyform;
            if fModifyForm
              then ModifyProperties
              else
                if csDesigning in ComponentState
                  then GenerateLanguageFile;
          end;
    end;

  procedure TInternationalizerComponent.Loaded;
    begin
      inherited;
    end;

  procedure TInternationalizerComponent.GenerateLanguageFile;
    var
      StringFileLines : TStringList;

    function ReadSimpleProperty(Component : TComponent; PropInfo : PPropInfo) : string;
      begin
        if (PropInfo.PropType^.Kind = tkString) or (PropInfo.PropType^.Kind = tkLString)
          then Result := GetStrProp(Component, PropInfo)
          else Result := '';
      end;

    procedure ReadStringsProperty(Component : TComponent; PropInfo : PPropInfo);
      var
        i       : integer;
        Obj     : TObject;
        Strings : TStrings;
      begin
        GetObjectProp(Component, PropInfo, Obj);
        Strings := TStrings(Obj);
        if (Strings <> nil) and (Strings.Count > 0)
          then
            begin
              StringFileLines.Add(Component.Name + '.' + PropInfo.Name + '=' + IntToStr(Strings.Count));
              for i := 0 to pred(Strings.Count) do
                StringFileLines.Add(Component.Name + '.' + PropInfo.Name + '.' + IntToStr(i) + '=' + Strings[i]);
            end;
      end;

    procedure ReadListColumnsProperty(Component : TComponent; PropInfo : PPropInfo);
      var
        i       : integer;
        Obj     : TObject;
        Columns : TListColumns;
      begin
        GetObjectProp(Component, PropInfo, Obj);
        Columns := TListColumns(Obj);
        if (Columns <> nil) and (Columns.Count > 0)
          then
            begin
              StringFileLines.Add(Component.Name + '.' + PropInfo.Name + '=' + IntToStr(Columns.Count));
              for i := 0 to pred(Columns.Count) do
                StringFileLines.Add(Component.Name + '.' + PropInfo.Name + '.' + IntToStr(i) + '=' + Columns[i].Caption);
            end;
      end;

    procedure ReadPropertyValues(Component : TComponent);
      var
        i        : integer;
        typinfo  : PTypeInfo;
        pc       : integer;
        proplist : PPropList;
      begin
        typinfo := Component.ClassInfo;
        if typinfo <> nil
          then
            begin
              proplist := nil;
              pc := GetPropList(typinfo, cValidPropTypes, proplist);
              getmem(proplist, pc*sizeof(PPropInfo));
              try
                GetPropList(typinfo, cValidPropTypes, proplist);
                for i := 0 to pred(pc) do
                  begin
                    if (fPropertyNames.IndexOf(proplist[i].Name) <> -1) or fReadAll
                      then
                        begin
                          if proplist[i].PropType^.Kind in cSimplePropTypes
                            then StringFileLines.Add(Component.Name + '.' + proplist[i].Name + '=' + ReadSimpleProperty(Component, proplist[i]))
                            else
                              if (proplist[i].PropType^.Kind = tkClass)
                                then
                                  if proplist[i].PropType^.Name = 'TStrings'
                                    then ReadStringsProperty(Component, proplist[i])
                                    else
                                      if proplist[i].PropType^.Name = 'TListColumns'
                                        then ReadListColumnsProperty(Component, proplist[i]);
                        end;
                  end;
              finally
                freemem(proplist);
              end;
            end;
        for i := 0 to pred(Component.ComponentCount) do
          ReadPropertyValues(Component.Components[i]);
      end;

    var
      Root : TComponent;
    begin
      try
        Root := Owner;
        if Root <> nil
          then
            begin
              while (Root.Owner <> nil) and not ((Root is TForm) or (Root is TVisualControl)) and (Root.Owner <> Application) do
                Root := Root.Owner;
              StringFileLines := TStringList.Create;
              try
                ReadPropertyValues(Root);
                if fTransFilesPath <> ''
                  then StringFileLines.SaveToFile(fTransFilesPath + IntToStr(fLanguageId) + '\' + Root.Name + '.tln')
                  else StringFileLines.SaveToFile(Root.Name + '.tln');
              finally
                StringFileLines.Free;
              end;
            end;
      except
      end;
    end;

  procedure TInternationalizerComponent.ModifyProperties;
    var
      StringFileLines : TStringList;
      i               : integer;
      compname        : string;
      propname        : string;
      propvalue       : string;
      Root            : TComponent;
      CurComponent    : TComponent;
      PropInfo        : PPropInfo;
      valuecount      : integer;

    function FindComponentInTree(Root : TComponent; const Name : string) : TComponent;
      var
        i : integer;
      begin
        if Name = Root.Name
          then Result := Root
          else
            begin
              i := 0;
              Result := nil;
              while (Result = nil) and (i < Root.ComponentCount) do
                begin
                  Result := FindComponentInTree(Root.Components[i], Name);
                  inc(i);
                end;
            end;
      end;

    procedure ParseFileLine(const fileline : string; out compname, propname, propvalue : string);
      var
        tmp : string;
      begin
        tmp := fileline;
        compname := copy(tmp, 1, pos('.', tmp) - 1);
        delete(tmp, 1, pos('.', tmp));
        propname := copy(tmp, 1, pos('=', tmp) - 1);
        delete(tmp, 1, pos('=', tmp));
        propvalue := tmp;
      end;

    procedure WriteSimpleProperty(Component : TComponent; PropInfo : PPropInfo; const PropValue : string);
      begin
        case PropInfo.PropType^.Kind of
          tkString, tkLString:
            SetStrProp(Component, PropInfo, PropValue);
        end;
      end;

    procedure LoadStringsProperty(Component : TComponent; PropInfo : PPropInfo; curidx, strcount : integer);
      var
        Obj     : TObject;
        Strings : TStrings;
        i       : integer;
      begin
        try
          GetObjectProp(Component, PropInfo, Obj);
          Strings := TStrings(Obj);
          if Strings <> nil
            then
              begin
                Strings.Clear;
                for i := curidx to pred(curidx + strcount) do
                  begin
                    ParseFileLine(StringFileLines[i], compname, propname, propvalue);
                    Strings.Add(propvalue);
                  end;
              end;
        except
        end;
      end;

    procedure LoadColumnsProperty(Component : TComponent; PropInfo : PPropInfo; curidx, colcount : integer);
      var
        Obj     : TObject;
        Columns : TListColumns;
        i, j    : integer;
      begin
        try
          GetObjectProp(Component, PropInfo, Obj);
          Columns := TListColumns(Obj);
          if Columns <> nil
            then
              begin
                j := 0;
                for i := curidx to pred(curidx + colcount) do
                  begin
                    ParseFileLine(StringFileLines[i], compname, propname, propvalue);
                    Columns[j].Caption := propvalue;
                    inc(j);
                  end;
              end;
        except
        end;
      end;

    begin
      try
        Root := Owner;
        if (Root <> nil) and FileExists(fTransFilesPath + IntToStr(fLanguageId) + '\' + Root.Name + '.tln')
          then
            begin
              while (Root.Owner <> nil) and not ((Root is TForm) or (Root is TVisualControl)) and (Root.Owner <> Application) do
                Root := Root.Owner;
              StringFileLines := TStringList.Create;
              try
                StringFileLines.LoadFromFile(fTransFilesPath + IntToStr(fLanguageId) + '\' + Root.Name + '.tln');
                CurComponent := nil;
                i := 0;
                while i < StringFileLines.Count do
                  begin
                    ParseFileLine(StringFileLines[i], compname, propname, propvalue);
                    if (CurComponent = nil) or (compname <> CurComponent.Name)
                      then CurComponent := FindComponentInTree(Root, compname);
                    if (CurComponent <> nil) and (CurComponent.ClassInfo <> nil)
                      then
                        begin
                          PropInfo := GetPropInfo(CurComponent.ClassInfo, propname);
                          if (PropInfo <> nil)
                            then
                              if PropInfo.PropType^.Kind in cSimplePropTypes
                                then
                                  begin
                                    WriteSimpleProperty(CurComponent, PropInfo, propvalue);
                                    inc(i);
                                  end
                                else
                                  begin
                                    valuecount := StrToInt(propvalue);
                                    if PropInfo.PropType^.Name = 'TStrings'
                                      then LoadStringsProperty(CurComponent, PropInfo, i + 1, valuecount)
                                      else
                                        if PropInfo.PropType^.Name = 'TListColumns'
                                          then LoadColumnsProperty(CurComponent, PropInfo, i + 1, valuecount);
                                    inc(i, valuecount + 1);
                                  end
                            else inc(i);
                        end
                      else inc(i);
                  end;
              finally
                StringFileLines.Free;
              end;
            end;
      except
      end;
    end;

  procedure Register;
    begin
      RegisterComponents('Samples', [TInternationalizerComponent]);
    end;

  procedure RegisterInternationalizer(Internationalizer : TInternationalizerComponent);
    begin
      Internationalizer.TransFilesPath := vTransFilesPath;
      Internationalizer.LanguageId := vLanguageId;
      if vTransFilesPath <> ''
        then Internationalizer.ModifyForm := true;
      vInternationalizerComps.Add(Internationalizer);
    end;

  procedure SetBasePath(const transfilespath : string);
    var
      i : integer;
    begin
      vTransFilesPath := transfilespath;
      for i := 0 to pred(vInternationalizerComps.Count) do
        TInternationalizerComponent(vInternationalizerComps[i]).TransFilesPath := transfilespath;
    end;

  procedure SetLanguage(languageid : integer);
    var
      i : integer;
    begin
      vLanguageId := languageid;
      for i := 0 to pred(vInternationalizerComps.Count) do
        begin
          TInternationalizerComponent(vInternationalizerComps[i]).LanguageId := languageid;
          if vTransFilesPath <> ''
            then TInternationalizerComponent(vInternationalizerComps[i]).ModifyForm := true;
        end;
    end;

initialization
  vInternationalizerComps := TList.Create;
finalization
  vInternationalizerComps.Free;
end.

