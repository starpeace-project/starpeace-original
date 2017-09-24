unit BlockStudies;

interface

  uses
    Collection, IniFiles, Classes;

  type
    TCollection = Collection.TCollection;

  const
    UnknownPrice = 0;

  var
    TradeCenterPrice : single  = 2;
    ROITime          : integer = 2*24*365;

  type
    TFluidStudy          = class;
    TFluidListStudy      = class;
    TBlockStudy          = class;
    TBlockListStudy      = class;
    TInputStudy          = class;
    TBasicInputStudy     = class;
    TAditionalInputStudy = class;
    TOutputStudy         = class;
    TStudy               = class;

    TFluidStudy  =
      class
        public
          constructor Create(aName, anId : string; aPrice : currency; aMeasure : string);
        private
          fName    : string;
          fId      : string;
          fPrice   : currency;
          fMeasure : string;
        public
          property Name    : string   read fName;
          property Id      : string   read fId;
          property Price   : currency read fPrice;
          property Measure : string   read fMeasure;
      end;

    TFluidListStudy =
      class
        public
          constructor Create(aFolder : string; aOwner : TStudy);
        private
          procedure InsertFluid(path : string);
        private
          fOwner  : TStudy;
          fFluids : TCollection;
        private
          function GetCount : integer;
          function GetFluid(index : integer) : TFluidStudy;
          function GetFluidByName(name : string) : TFluidStudy;
        public
          property Count : integer read GetCount;
          property Fluids[index : integer] : TFluidStudy read GetFluid; default;
          property FluidByName[name : string] : TFluidStudy read GetFluidByName;
      end;

    TBlockStudy  =
      class
        public
          constructor Create(IniFile : TIniFile; TheFluids : TFluidListStudy);
          destructor  Destroy; override;
        protected
          constructor CreateEmpty;
        private
          fName            : string;
          fBudget          : currency;
          fCost            : currency;
          fAditionalInputs : TCollection;
          fBasicInputs     : TCollection;
          fOutputs         : TCollection;
          fProfit          : currency;
          fRecoveryTime    : single;
          fCluster         : string;
          fFileName        : string;
          fLattency        : single;
          fLowOperation    : single;
          fCloned          : boolean;
        private
          function GetBasicInputCount : integer;
          function GetAditionalInputCount : integer;
          function GetOutputCount : integer;
          function GetBasicInput(index : integer) : TBasicInputStudy;
          function GetAditionalInput(index : integer) : TAditionalInputStudy;
          function GetOutput(index : integer) : TOutputStudy;
        public
          property Name            : string   read fName;
          property Budget          : currency read fBudget;
          property Cost            : currency read fCost;
          property RecoveryTime    : single read fRecoveryTime;
          property Profit          : currency read fProfit;
          property Cluster         : string read fCluster;
          property FileName        : string read fFileName;
          property BasicInputCount : integer  read GetBasicInputCount;
          property AditionalInputCount : integer read GetAditionalInputCount;
          property OutputCount : integer read GetOutputCount;
          property BasicInputs[index : integer] : TBasicInputStudy read GetBasicInput;
          property AditionalInputs[index : integer] : TAditionalInputStudy read GetAditionalInput;
          property Outputs[index : integer] : TOutputStudy read GetOutput;
          property LowOperation : single read fLowOperation;
          property Cloned : boolean read fCloned write fCloned;
        public
          procedure Evaluate(ROI : double);
          function  FindInput(aName : string) : TInputStudy;
        public
          function Clone(inputFactor, outputFactor, growFactor : single; small : boolean) : TBlockStudy;
          procedure Save(cluster, path : string; small : boolean);
      end;

    TBlockListStudy =
      class
        public
          constructor Create(aFolder : string; aOwner : TStudy);
        private
          procedure InsertBlock(path : string);
        private
          fOwner  : TStudy;
          fBlocks : TCollection;
        private
          function GetCount : integer;
          function GetBlock(index : integer) : TBlockStudy;
          function GetBlockByName(name : string) : TBlockStudy;
        public
          property Count : integer read GetCount;
          property Blocks[index : integer] : TBlockStudy read GetBlock; default;
          property BlockByName[name : string] : TBlockStudy read GetBlockByName;
      end;

    TGateStudy  =
      class
        public
          constructor Create(aName  : string;
                             aPrice : currency;
                             aMax   : single;
                             aFluid : TFluidStudy);
        private
          fName  : string;
          fPrice : currency;
          fMax   : single;
          fFluid : TFluidStudy;
          fImportable : boolean;
          fIsWorkforce : boolean;
        public
          property Name  : string      read fName;
          property Price : currency    read fPrice;
          property Max   : single      read fMax;
          property Fluid : TFluidStudy read fFluid;
          property Importable : boolean  read fImportable;
        public
          function Clone : TGateStudy;
      end;

    TInputStudy  =
      class(TGateStudy)
      end;

    TBasicInputStudy  =
      class(TInputStudy)
      end;

    TAditionalInputStudy  =
      class(TInputStudy)
      end;

    TOutputStudy =
      class(TGateStudy)
      end;

    TStudy =
      class
        public
          constructor Create(aRootPath : string);
          destructor Destroy; override;
        private
          fFluids : TFluidListStudy;
          fBlocks : TBlockListStudy;
        public
          property Fluids : TFluidListStudy read fFluids;
          property Blocks : TBlockListStudy read fBlocks;
        public
          procedure Evaluate;
          procedure GenerateConsts(const path, cluster : string);
          function  GetDependencies(aBlock : TBlockStudy) : TStringList;
      end;

implementation

  uses
    SysUtils, MathUtils;

  // TFluidStudy

  constructor TFluidStudy.Create(aName, anId : string; aPrice : currency; aMeasure : string);
    begin
      inherited Create;
      fName := aName;
      fId   := anId;
      fPrice := aPrice;
      fMeasure := aMeasure;
    end;

  // TFluidListStudy

  constructor TFluidListStudy.Create(aFolder : string; aOwner : TStudy);
    var
      Search : TSearchRec;
    begin
      inherited Create;
      fOwner  := aOwner;
      fFluids := TCollection.Create(0, rkBelonguer);
      if FindFirst(aFolder + '*.ini', faArchive, Search) = 0
        then
          repeat
            InsertFluid(aFolder + Search.Name);
          until FindNext(Search) <> 0;
      FindClose( Search );
    end;

  procedure TFluidListStudy.InsertFluid(path : string);
    var
      IniFile : TIniFile;
      FluidId : string;
    begin
      IniFile := TIniFile.Create(path);
      FluidId := ExtractFileName(path);
      FluidId := copy( FluidId, 1, length(FluidId) - length('.ini') );
      fFluids.Insert(
        TFluidStudy.Create(
          IniFile.ReadString('General', 'Name', 'Unknown'),
          FluidId,
          IniFile.ReadInteger('General', 'Price', UnknownPrice),
          IniFile.ReadString('General', 'Measure', '')));
      IniFile.Free;
    end;

  function TFluidListStudy.GetCount : integer;
    begin
      result := fFluids.Count;
    end;

  function TFluidListStudy.GetFluid(index : integer) : TFluidStudy;
    begin
      result := TFluidStudy(fFluids[index]);
    end;

  function TFluidListStudy.GetFluidByName(name : string) : TFluidStudy;
    var
      i : integer;
    begin
      i := pred(Count);
      while (i >= 0) and (Fluids[i].Id <> name) do
        dec(i);
      if i >= 0
        then result := Fluids[i]
        else result := nil;
    end;

  // TBlockStudy

  constructor TBlockStudy.Create(IniFile : TIniFile; TheFluids : TFluidListStudy);
    var
      count : integer;
      i     : integer;
    begin
      inherited Create;
      fAditionalInputs := TCollection.Create(0, rkBelonguer);
      fBasicInputs     := TCollection.Create(0, rkBelonguer);
      fOutputs         := TCollection.Create(0, rkBelonguer);
      fFileName        := IniFile.FileName;
      // Read general data from ini file
      fName    := IniFile.ReadString('General', 'Name', 'Unknown');
      fBudget  := StrToCurr(IniFile.ReadString('General', 'Budget', '0'));
      fCost    := StrToCurr(IniFile.ReadString('General', 'Cost', '0'));
      fCluster := lowercase(IniFile.ReadString('General', 'Cluster', ''));
      //fLattency := StrToFloat(IniFile.ReadString('General', 'Lattency', '1'));
      fLattency := 1;
      fCloned := IniFile.ReadBool('General', 'cloned', false);
      // Read basic inputs
      count := IniFile.ReadInteger('Basic inputs', 'Count', 0);
      for i := 0 to pred(count) do
        fBasicInputs.Insert(
          TBasicInputStudy.Create(
            IniFile.ReadString('basic_input_' + IntToStr(i), 'Name', 'Unknown'),
            StrToCurr(IniFile.ReadString('basic_input_' + IntToStr(i), 'Price', '0')),
            StrToFloat(IniFile.ReadString('basic_input_' + IntToStr(i), 'Max', '0')),
            TheFluids.FluidByName[IniFile.ReadString('basic_input_' + IntToStr(i), 'Fluid', 'Unknown')]));
      // Read aditional inputs
      count := IniFile.ReadInteger('Aditional inputs', 'Count', 0);
      for i := 0 to pred(count) do
        fAditionalInputs.Insert(
          TAditionalInputStudy.Create(
            IniFile.ReadString('adt_input_' + IntToStr(i), 'Name', 'Unknown'),
            StrToCurr(IniFile.ReadString('adt_input_' + IntToStr(i), 'Price', '0')),
            StrToFloat(IniFile.ReadString('adt_input_' + IntToStr(i), 'Max', '0')),
            TheFluids.FluidByName[IniFile.ReadString('adt_input_' + IntToStr(i), 'Fluid', 'Unknown')]));
      // Read outputs
      count := IniFile.ReadInteger('Outputs', 'Count', 0);
      for i := 0 to pred(count) do
        fOutputs.Insert(
          TOutputStudy.Create(
            IniFile.ReadString('output_' + IntToStr(i), 'Name', 'Unknown'),
            StrToCurr(IniFile.ReadString('output_' + IntToStr(i), 'Price', '0')),
            StrToFloat(IniFile.ReadString('output_' + IntToStr(i), 'Max', '0')),
            TheFluids.FluidByName[IniFile.ReadString('output_' + IntToStr(i), 'Fluid', 'Unknown')]));
    end;

  destructor TBlockStudy.Destroy;
    begin
      fAditionalInputs.Free;
      fBasicInputs.Free;
      fOutputs.Free;
      inherited;
    end;

  constructor TBlockStudy.CreateEmpty;
    begin
      inherited Create;
      fAditionalInputs := TCollection.Create(0, rkBelonguer);
      fBasicInputs     := TCollection.Create(0, rkBelonguer);
      fOutputs         := TCollection.Create(0, rkBelonguer);
      fLattency        := 1;
    end;

  function TBlockStudy.Clone(inputFactor, outputFactor, growFactor : single; small : boolean) : TBlockStudy;
    var
      i        : integer;
      CurrGate : TGateStudy;
      NewGate  : TGateStudy;
    begin
      result := TBlockStudy.CreateEmpty;
      result.fBudget := round(fBudget*outputFactor);
      if small
        then
          begin
            result.fName := fName + ' small';
            result.fCost := round(fCost*growFactor);
          end
        else
          begin
            result.fName := fName;
            result.fCost := round(fCost*(1.4*growFactor));
            //outputFactor := outputFactor + 0.05;
            //inputFactor  := inputFactor  - 0.05;
          end;
      result.fFileName := fFileName;
      result.fCluster := fCluster;
      // basic inputs
      for i := 0 to pred(fBasicInputs.Count) do
        begin
          CurrGate := TBasicInputStudy(fBasicInputs[i]);
          NewGate  := CurrGate.Clone;
          if CurrGate.fIsWorkforce
            then NewGate.fMax := SmartRound(CurrGate.fMax*sqrt(inputFactor))
              {
              if (CurrGate.fMax = 0) and not small
                then NewGate.fMax := SmartRound(inputFactor)
                else NewGate.fMax := SmartRound(CurrGate.fMax*inputFactor)
              }
            else NewGate.fMax := round(CurrGate.fMax*inputFactor);
          result.fBasicInputs.Insert(NewGate);
        end;

      // aditional inputs
      for i := 0 to pred(fAditionalInputs.Count) do
        begin
          CurrGate     := TAditionalInputStudy(fAditionalInputs[i]);
          NewGate      := CurrGate.Clone;
          NewGate.fMax := CurrGate.fMax*inputFactor;
          result.fAditionalInputs.Insert(NewGate);
        end;

      // outputs
      for i := 0 to pred(fOutputs.Count) do
        begin
          CurrGate     := TOutputStudy(fOutputs[i]);
          NewGate      := CurrGate.Clone;
          NewGate.fMax := CurrGate.fMax*outputFactor;
          result.fOutputs.Insert(NewGate);
        end;

      result.fCloned := true;
    end;

  procedure TBlockStudy.Save(cluster, path : string; small : boolean);
    var
      i    : integer;
      Gate : TGateStudy;
      Ini  : TIniFile;
    begin
      if small
        then Ini := TIniFile.Create(path + cluster + '.small.' + ExtractFileName(fFileName))
        else Ini := TIniFile.Create(path + cluster + '.' + ExtractFileName(fFileName));
      Ini.WriteString('General', #9+'name', fName);
      Ini.WriteString('General', #9+'cost', FloatToStr(fCost));
      Ini.WriteString('General', #9+'budget', FloatToStr(fBudget));
      Ini.WriteString('General', #9+'cluster', cluster + ^M^J);
      Ini.WriteBool('General', 'cloned', fCloned);
      Ini.WriteInteger('Basic inputs', 'count', fBasicInputs.Count);
      for i := 0 to pred(fBasicInputs.Count) do
        begin
          Gate := TGateStudy(fBasicInputs[i]);
          Ini.WriteString('basic_input_' + IntToStr(i), #9+'name', Gate.fName);
          Ini.WriteString('basic_input_' + IntToStr(i), #9+'fluid', Gate.fFluid.fId);
          Ini.WriteString('basic_input_' + IntToStr(i), #9+'max', FloatToStr(Gate.fMax) + ^M^J);
        end;

      Ini.WriteInteger('Aditional inputs', 'count', fAditionalInputs.Count);
      for i := 0 to pred(fAditionalInputs.Count) do
        begin
          Gate := TGateStudy(fAditionalInputs[i]);
          Ini.WriteString('adt_input_' + IntToStr(i), #9+'name', Gate.fName);
          Ini.WriteString('adt_input_' + IntToStr(i), #9+'fluid', Gate.fFluid.fId);
          Ini.WriteString('adt_input_' + IntToStr(i), #9+'max', FloatToStr(Gate.fMax) + ^M^J);
        end;

      Ini.WriteInteger('Outputs', 'count', fOutputs.Count);
      for i := 0 to pred(fOutputs.Count) do
        begin
          Gate := TGateStudy(fOutputs[i]);
          Ini.WriteString('output_' + IntToStr(i), #9+'name', Gate.fName);
          Ini.WriteString('output_' + IntToStr(i), #9+'fluid', Gate.fFluid.fId);
          Ini.WriteString('output_' + IntToStr(i), #9+'max', FloatToStr(Gate.fMax) + ^M^J);
        end;
    end;

  function TBlockStudy.GetBasicInputCount : integer;
    begin
      result := fBasicInputs.Count;
    end;

  function TBlockStudy.GetAditionalInputCount : integer;
    begin
      result := fAditionalInputs.Count;
    end;

  function TBlockStudy.GetOutputCount : integer;
    begin
      result := fOutputs.Count;
    end;

  function TBlockStudy.GetBasicInput(index : integer) : TBasicInputStudy;
    begin
      result := TBasicInputStudy(fBasicInputs[index]);
    end;

  function TBlockStudy.GetAditionalInput(index : integer) : TAditionalInputStudy;
    begin
      result := TAditionalInputStudy(fAditionalInputs[index]);
    end;

  function TBlockStudy.GetOutput(index : integer) : TOutputStudy;
    begin
      result := TOutputStudy(fOutputs[index]);
    end;

  procedure TBlockStudy.Evaluate(ROI : double);
    var
      i          : integer;
      InputCost  : single;
      OutputCost : single;
      WFCost     : single;
    begin
      InputCost  := 0;
      WFCost     := 0;
      for i := 0 to pred(BasicInputCount) do
        with BasicInputs[i] do
          begin
            if fIsWorkforce
              then WFCost := WFCost + Price * Max;
            if not Importable
              then InputCost := InputCost + TradeCenterPrice * Price * Max
              else InputCost := InputCost + Price * Max;
          end;
      for i := 0 to pred(AditionalInputCount) do
        with AditionalInputs[i] do
          begin
            if not Importable
              then InputCost := InputCost + TradeCenterPrice * Price * Max
              else InputCost := InputCost + Price * Max;
          end;
      OutputCost := 0;
      for i := 0 to pred(OutputCount) do
        OutputCost := OutputCost + Outputs[i].Price * Outputs[i].Max;
      //fProfit := OutputCost - InputCost - Budget;

      fProfit := OutputCost - InputCost;
      fLowOperation := WFCost*100/(OutputCost - WFCost);

      if fCloned
        then
          if fProfit <> 0
            then fRecoveryTime := fCost/(fProfit - fBudget)
            else fRecoveryTime := -98939291 // $%^%^$%#%#^$^##
        else
          if fProfit <> 0
            then fRecoveryTime := fCost/fProfit // - fBudget)
            else fRecoveryTime := -98939291; // $%^%^$%#%#^$^##

      if not fCloned
        then
          if fRecoveryTime < ROI
            then
              begin
                fBudget := fProfit - fCost/(ROI*fLattency);
                fRecoveryTime := fCost/(fProfit - fBudget);
              end
            else fBudget := 0;

      fProfit := fProfit - fBudget;
    end;

  function TBlockStudy.FindInput(aName : string) : TInputStudy;
    var
      i     : integer;
    begin
      aName := UpperCase(aName);
      i     := 0;
      while (i < BasicInputCount) and (aName <> UpperCase(BasicInputs[i].Name)) do
        inc(i);
      if i = BasicInputCount
        then
          begin
            while (i < AditionalInputCount) and (aName <> UpperCase(AditionalInputs[i].Name)) do
              inc(i);
            if i < AditionalInputCount
              then result := AditionalInputs[i]
              else result := nil;
          end
        else result := BasicInputs[i];
    end;

  // TBlockListStudy

  constructor TBlockListStudy.Create(aFolder : string; aOwner : TStudy);
    var
      Search : TSearchRec;
    begin
      inherited Create;
      fOwner  := aOwner;
      fBlocks := TCollection.Create(0, rkBelonguer);
      if FindFirst(aFolder + '*.ini', faArchive, Search) = 0
        then
          repeat
            InsertBlock(aFolder + Search.Name);
          until FindNext(Search) <> 0;
      FindClose( Search );
    end;

  procedure TBlockListStudy.InsertBlock(path : string);
    var
      IniFile : TIniFile;
    begin
      IniFile := TIniFile.Create(path);
      fBlocks.Insert(TBlockStudy.Create(IniFile, fOwner.Fluids));
      IniFile.Free;
    end;

  function TBlockListStudy.GetCount : integer;
    begin
      result := fBlocks.Count;
    end;

 function TBlockListStudy.GetBlock(index : integer) : TBlockStudy;
    begin
      result := TBlockStudy(fBlocks[index]);
    end;

  function TBlockListStudy.GetBlockByName(name : string) : TBlockStudy;
    var
      i : integer;
    begin
      i := pred(Count);
      while (i >= 0) and (Blocks[i].Name <> name) do
        dec(i);
      if i >= 0
        then result := Blocks[i]
        else result := nil;
    end;

  // TGateStudy

  constructor TGateStudy.Create(aName  : string;
                                aPrice : currency;
                                aMax   : single;
                                aFluid : TFluidStudy);
    begin
      fName  := aName;
      if aPrice <> UnknownPrice
        then fPrice := aPrice
        else
          if aFluid <> nil
            then fPrice := aFluid.Price
            else fPrice := 0;
      fMax   := aMax;
      fFluid := aFluid;
      if fFluid <> nil
        then
          begin
            fIsWorkforce := (fFluid.fId = 'hiWorkForce') or (fFluid.fId = 'midWorkForce') or (fFluid.fId = 'loWorkForce');
            fImportable  := fIsWorkforce or (fFluid.fId = 'FreshFood') or (fFluid.fId = 'CompServ') or (fFluid.fId = 'LegalServ');
          end;
    end;

  function TGateStudy.Clone : TGateStudy;
    begin
      result              := TGateStudy.Create(fName, fPrice, fMax, fFluid);
      result.fImportable  := fImportable;
      result.fIsWorkforce := fIsWorkforce;
    end;

  // TStudy

  constructor TStudy.Create(aRootPath : string);
    begin
      inherited Create;
      fFluids := TFluidListStudy.Create(aRootPath + 'Fluids\', Self);
      fBlocks := TBlockListStudy.Create(aRootPath, Self);
    end;

  destructor TStudy.Destroy;
    begin
      fFluids.Free;
      fBlocks.Free;
      inherited;
    end;

  procedure TStudy.Evaluate;
    var
      i : integer;
    begin
      for i := 0 to pred(fBlocks.Count) do
        fBlocks[i].Evaluate(ROITime);
    end;

  function CleanSpaces(const str : string) : string;
    var
      i, j : integer;
    begin
      j := 0;
      result := str;
      for i := 1 to length(str) do
        if str[i] <> ' '
          then
            begin
              inc(j);
              result[j] := str[i];
            end;
      SetLength(result, j);
    end;

  procedure TStudy.GenerateConsts(const path, cluster : string);
    var
      Block   : TBlockStudy;
      i       : integer;
      index   : integer;
      TheUnit : TextFile;
      BlkName : string;
      UnitName : string;
    begin
      assign(TheUnit, path);
      rewrite(TheUnit);
      UnitName := ExtractFileName(path);
      SetLength(UnitName, length(UnitName) - length(ExtractFileExt(path)));
      WriteLn(TheUnit, 'unit ' + UnitName + ';');
      WriteLn(TheUnit);
      WriteLn(TheUnit, 'interface');
      WriteLn(TheUnit);
      WriteLn(TheUnit, '  const');
      for i := 0 to pred(fBlocks.Count) do
        begin
          Block := fBlocks[i];
          if Block.fCluster = lowercase(cluster)
            then
              begin
                BlkName := Block.Name;
                WriteLn(TheUnit);
                WriteLn(TheUnit, '    // ' + BlkName);
                BlkName := CleanSpaces(BlkName);
                index   := pos(lowercase(cluster), lowercase(BlkName));
                if index <> 0
                  then delete(BlkName, index, length(cluster));
                WriteLn(TheUnit, '    cost_' + BlkName + ' = ' + CurrToStr(Block.Cost) + ';');
                WriteLn(TheUnit, '    budget_' + BlkName + ' = ' + CurrToStr(Block.Budget) + ';');
                for index := 0 to pred(Block.fBasicInputs.Count) do
                  with TInputStudy(Block.fBasicInputs[index]) do
                    WriteLn(TheUnit,
                      '    input_' +
                      BlkName + '_' +
                      CleanSpaces(fName) + ' = ' +
                      FloatToStr(fMax) + ';');
                for index := 0 to pred(Block.fAditionalInputs.Count) do
                  with TInputStudy(Block.fAditionalInputs[index]) do
                    WriteLn(TheUnit,
                      '    input_' +
                      BlkName + '_' +
                      CleanSpaces(fName) + ' = ' +
                      FloatToStr(fMax) + ';');
                WriteLn(TheUnit);
                for index := 0 to pred(Block.fOutputs.Count) do
                  with TOutputStudy(Block.fOutputs[index]) do
                    WriteLn(TheUnit,
                      '    output_' +
                      BlkName + '_' +
                      CleanSpaces(fName) + ' = ' +
                      FloatToStr(fMax) + ';');
                WriteLn(TheUnit);
              end;
        end;
      WriteLn(TheUnit, 'implementation');
      WriteLn(TheUnit);
      WriteLn(TheUnit, 'end.');
      close(TheUnit);
    end;

  function TStudy.GetDependencies(aBlock : TBlockStudy) : TStringList;
    var
      Indent : string;
      i, j   : integer;
      Input  : TInputStudy;
      InpSum : single;
      Prod   : single;
    begin
      Indent := '  ';
      result := TStringList.Create;
      for i := 0 to pred(aBlock.OutputCount) do
        begin
          Prod   := aBlock.Outputs[i].Max;
          InpSum := 0;
          result.Add(aBlock.Outputs[i].Name);
          for j := 0 to pred(Blocks.Count) do
            if uppercase(Blocks[j].fCluster) = uppercase(aBlock.fCluster)
              then
                begin
                  Input := Blocks[j].FindInput(aBlock.Outputs[i].Name);
                  if Input <> nil
                    then
                      begin
                        if Input.Max > Prod
                          then result.Add(Indent + Blocks[j].Name + ' Needs ' + IntToStr(round(Input.Max / Prod)) + ' ' + aBlock.Name)
                          else result.Add(Indent + 'Satisfies: ' + IntToStr(round(Prod / Input.Max)) + ' ' + Blocks[j].Name + ': ' + FloatToStr(Input.Max));
                        InpSum := InpSum + Input.Max;
                      end;
                end;
          result.Add(Indent + 'Produces: ' + FloatToStr(Prod));
          result.Add(Indent + 'Consume: ' + FloatToStr(InpSum));
          result.Add('');
        end;
    end;

end.

