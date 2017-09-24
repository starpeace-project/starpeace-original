program InvMaker;

uses
  Classes,
  SysUtils,
  ComObj,
  DelphiStreamUtils in '..\Utils\Misc\DelphiStreamUtils.pas';

{$R *.RES}

  const
    tidMSXML = 'msxml';

  const
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

  procedure ConvertInvention(InvObj : OleVariant; destPath : string);
    var
      name  : string;
      kind  : string;
      id    : string;
      price : integer;
      effic : integer;
      q     : integer;
      prest : integer;
      desc  : string;
      base  : boolean;
      DsObj : OleVariant;
      St    : TStream;
    begin
      name  := InvObj.getAttribute(tidInvAttr_Name);
      kind  := InvObj.getAttribute(tidInvAttr_Kind);
      id    := InvObj.getAttribute(tidInvAttr_Id);
      price := InvObj.getAttribute(tidInvAttr_Price);
      effic := InvObj.getAttribute(tidInvAttr_Effic);
      q     := InvObj.getAttribute(tidInvAttr_Q);
      prest := InvObj.getAttribute(tidInvAttr_Prestige);
      base  := lowercase(InvObj.getAttribute(tidInvAttr_BaseTech)) = 'true';
      if not VarIsEmpty(InvObj.children)
        then
          begin
            DsObj := InvObj.children.item(tidInvElement_Desc, Unassigned);
            if not VarIsEmpty(DsObj)
              then desc := DsObj.text
              else desc := '';
          end
        else desc := '';
      St := TFileStream.Create(destPath + id + '.inv', fmCreate);
      DelphiStreamUtils.WriteString(St, name);
      DelphiStreamUtils.WriteString(St, kind);
      St.Write(price, sizeof(price));
      St.Write(effic, sizeof(effic));
      St.Write(q, sizeof(q));
      St.Write(prest, sizeof(prest));
      DelphiStreamUtils.WriteString(St, desc);
      St.Write(base, sizeof(base));
    end;

  procedure ConvertInventionsFromFile(filePath, destPath : string);
    var
      InvFile : OleVariant;
      Root    : OleVariant;
      i       : integer;
      InvObj  : OleVariant;
      Invtns  : OleVariant;
    begin
      try
        InvFile := CreateOLEObject(tidMSXML);
        InvFile.url := filePath;
        Root   := InvFile.root;
        Invtns := Root.children;
        i      := 0;
        while i < Invtns.Length do
          begin
            InvObj := Invtns.item(i, Unassigned);
            ConvertInvention(InvObj, destPath);
            inc(i);
          end;
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

begin
  if ParamCount >= 2
    then ConvertInventions(ParamStr(1), ParamStr(2));
end.
