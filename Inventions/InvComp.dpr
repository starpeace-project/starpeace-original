program InvComp;

uses
  Classes,
  SysUtils,
  ComObj,
  DelphiStreamUtils in '..\Utils\Misc\DelphiStreamUtils.pas',
  CompStringsParser in '..\Utils\Misc\CompStringsParser.pas',
  MathUtils in '..\Utils\Misc\MathUtils.pas';

{$R *.RES}

  const
    tidMSXML = 'msxml';

  const
    tidInvElem_Properties  = 'PROPERTIES';
    tidInvElem_Requires    = 'REQUIRES';
    tidInvAttr_Kind        = 'kind';
    tidInvAttr_Id          = 'id';
    tidInvAttr_Name        = 'name';
    tidInvAttr_Location    = 'location';
    tidInvAttr_Price       = 'price';
    tidInvAttr_Effic       = 'effic';
    tidInvAttr_Q           = 'Q';
    tidInvAttr_Prestige    = 'prestige';
    tidInvElement_Requires = 'requires';
    tidInvElement_Desc     = 'description';
    tidInvAttr_BaseTech    = 'basetech';
    tidInvElement_Prop     = 'properties';
    tidInvAttr_Parent      = 'parent';
    tidInvAttr_Value       = 'value';
    tidInvAttr_File        = 'file';
    tidInvAttr_Cached      = 'cache';


  var
    InvNames : TStringList;

  type
    TPropClass =
      class
        public
          constructor Create(clsObj : OleVariant);
        private
          fId     : string;
          fName   : string;
          fType   : string;
          fVisual : boolean;
          fUnits  : string;
          fOrder  : integer;
        public
          function Format(value : string) : string;
      end;

  // TPropClass

  constructor TPropClass.Create(clsObj : OleVariant);
    begin
      inherited Create;
      fId     := clsObj.getAttribute('id');
      fName   := clsObj.getAttribute('name');
      fType   := clsObj.getAttribute('type');
      fVisual := lowercase(clsObj.getAttribute('visual')) = 'yes';
      fUnits  := clsObj.getAttribute('units');
      fOrder  := clsObj.getAttribute('order');
    end;

  function GetSignedInt(str : string) : string;
    var
      val : integer;
    begin
      try
        val := StrToInt(str);
        if val = 0
          then result := '0'
          else
            if val > 0
              then result := '+' + str
              else result := str;
      except
        str := '0';
      end;
    end;

  function TPropClass.Format(value : string) : string;
    begin
      if (fType = '') or (fType = 'string')
        then result := value
        else
          if fType = 'percent'
            then result := GetSignedInt(value) + '%'
            else
              if fType = 'int'
                then
                  if fUnits <> ''
                    then result := GetSignedInt(value) + ' ' + fUnits
                    else result := GetSignedInt(value)
                else
                  if fType = 'currency'
                    then result := FormatMoneyStr(value)
                    else
                      if fType = 'tier'
                        then
                          if value <> ''
                            then
                              case StrToInt(value) of
                                0: result := 'Apprentice';
                                1: result := 'Entrepreneur';
                                2: result := 'Tycoon';
                                3: result := 'Master';
                                4: result := 'Paradigm';
                                5: result := 'Legend';
                              end
                            else result := 'Apprentice'
                        else result := value;
      result := fName + ': ' + result;
    end;


  var
    theClasses : TStringList;
    Files      : TStringList;

  function FindClass(ppId : string) : TPropClass;
    var
      idx : integer;
    begin
      idx := theClasses.IndexOf(ppId);
      if idx <> -1
        then result := TPropClass(theClasses.Objects[idx])
        else result := nil;
    end;

  function FormatProp(ppName, ppValue : string) : string;
    var
      ppClass : TPropClass;
    begin
      ppClass := FindClass(ppName);
      if ppClass <> nil
        then result := ppClass.Format(ppValue)
        else result := ppName + ': ' + ppValue;
    end;

  function GetProperty(ppObj : OleVariant; name : string) : OleVariant;
    var
      Aux : OleVariant;
    begin
      Aux := ppObj.children.item(UpperCase(name), Unassigned);
      if VarIsEmpty(Aux)
        then result := ppObj.getAttribute(tidInvAttr_Value)
        else result := Aux.getAttribute(tidInvAttr_Value);
    end;

  procedure ReadClasses(Path : string);
    var
      clsFile : OleVariant;
      Root    : OleVariant;
      clsObj  : OleVariant;
      i, l    : integer;
      newClass : TPropClass;
    begin
      clsFile := CreateOLEObject(tidMSXML);
      clsFile.url := Path;
      Root := clsFile.root;
      l := Root.children.length;
      for i := 0 to pred(l) do
        begin
          clsObj   := Root.children.item(i, Unassigned);
          newClass := TPropClass.Create(clsObj);
          theClasses.AddObject(newClass.fId, newClass);
        end;
    end;

  procedure ReadLink(Path : string; Obj : OleVariant);
    var
      flName : string;
    begin
      flName := Obj.getAttribute(tidInvAttr_File);
      if Files.IndexOf(flName) = -1
        then
          begin
            ReadClasses(Path + flName);
            Files.Add(flName);
          end;
    end;

  function GetRequiredInvention(ppObj : OleVariant) : string;
    var
      Aux : OleVariant;
      i   : integer;
      req : string;
      id  : string;
    begin
      result := '';
      if not VarIsEmpty(ppObj)
        then
          begin
            result := '';
            for i := 0 to ppObj.children.length - 1 do
              begin
                Aux := ppObj.children.item(i, Unassigned);
                id  := Aux.getAttribute(tidInvAttr_Id);
                req := InvNames.Values[id];
                if result <> ''
                  then result := result + ', ' + req
                  else result := req;
              end;
          end;
    end;

  procedure ConvertInvention(InvObj : OleVariant; destPath : string);
    var
      cached : string;
      name   : string;
      id     : string;
      prnt   : string;
      desc   : string;
      base   : boolean;
      Aux    : OleVariant;
      St     : TStream;
      props  : string;
      reqStr : string;
      ppObj  : OleVariant;
      Prop   : OleVariant;
      ppCls  : TPropClass;
      i      : integer;
      ppList : TStringList;

      procedure InsertProp(Prop : string; order : integer);
        var
          idx : integer;
        begin
          idx := 0;
          while (idx < ppList.Count) and (order >= integer(ppList.Objects[idx])) do
            inc(idx);
          ppList.Insert(idx, Prop);
          ppList.Objects[idx] := TObject(order);
        end;

    begin
      ppObj  := InvObj.children.item(tidInvElem_Properties, Unassigned);
      cached := GetProperty(ppObj, tidInvAttr_Cached);
      name   := GetProperty(ppObj, tidInvAttr_Name);
      id     := GetProperty(ppObj, tidInvAttr_Id);
      InvNames.Values[id] := name;
      if UpperCase(cached) <> 'NO'
        then
          begin
            base  := lowercase(GetProperty(ppObj, tidInvAttr_BaseTech)) = 'true';
            prnt  := GetProperty(ppObj, tidInvAttr_Parent);
            if not VarIsEmpty(InvObj.children)
              then
                begin
                  Aux := InvObj.children.item(tidInvElement_Desc, Unassigned);
                  if not VarIsEmpty(Aux)
                    then desc := Aux.text
                    else desc := '';
                end
              else desc  := '';

            // requires
            reqStr := GetRequiredInvention(InvObj.children.item(tidInvElem_Requires, Unassigned));
            if reqStr <> ''
              then desc := desc + ^M^J + 'Requires: ' + reqStr + '.';

            // Generation of the file
            St := TFileStream.Create(destPath + id + '.inv', fmCreate);
            DelphiStreamUtils.WriteString(St, name);
            DelphiStreamUtils.WriteString(St, desc);
            DelphiStreamUtils.WriteString(St, prnt);
            props  := '';
            ppList := TStringList.Create;
            for i := 0 to ppObj.children.Length - 1 do
              begin
                Prop  := ppObj.children.item(i, Unassigned);
                ppCls := FindClass(Prop.tagName);
                if (ppCls <> nil) and ppCls.fVisual
                  then InsertProp(ppCls.Format(Prop.getAttribute(tidInvAttr_Value)), ppCls.fOrder);
              end;
            props := ppList.Text;
            ppList.Free;
            DelphiStreamUtils.WriteString(St, props);
            St.Write(base, sizeof(base));
            St.Free;
          end;
    end;

  procedure ListInventionFileElements(filePath, destPath : string; Elems : OleVariant);
    var
      i, len : integer;
      Obj    : OleVariant;
    begin
      i := 0;
      len := Elems.Length;
      while i < len do
        begin
          Obj := Elems.item(i, Unassigned);
          if Obj.tagName = 'INVENTIONSET'
            then ListInventionFileElements(filePath, destPath, Obj.children)
            else
              if Obj.tagName = 'INVENTION'
                then ConvertInvention(Obj, destPath)
                else
                  if Obj.tagName = 'LINK'
                    then ReadLink(ExtractFilePath(filePath), Obj);
          inc(i);
        end;
    end;

  procedure ConvertInventionsFromFile(filePath, destPath : string);
    var
      InvFile : OleVariant;
      Root    : OleVariant;
    begin
      try
        InvFile := CreateOLEObject(tidMSXML);
        InvFile.url := filePath;
        Root := InvFile.root;
        ListInventionFileElements(filePath, destPath, Root.children);
      except
        raise Exception.Create('Error reading the invention file: ' + ExtractFileName(filePath));
      end;
    end;

  procedure ConvertInventions(sourcePath, destPath : string);
    var
      Search : TSearchRec;
      found  : integer;
    begin
      found := FindFirst(SourcePath + '*.xml', faArchive, Search);
      while found = 0 do
        begin
          ConvertInventionsFromFile(sourcePath + Search.Name, DestPath);
          found := FindNext(Search);
        end;
    end;

  procedure ReleaseClasses;
    var
      i : integer;
    begin
      for i := 0 to pred(theClasses.Count) do
        theClasses.Objects[i].Free;
    end;

begin
  theClasses := TStringList.Create;
  Files      := TStringList.Create;
  InvNames   := TStringList.Create;
  if ParamCount >= 2
    then ConvertInventions(ParamStr(1), ParamStr(2));
  ReleaseClasses;
  theClasses.Free;
  Files.Free;
end.
