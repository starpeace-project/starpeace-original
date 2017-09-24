library HelpExpert;

  uses
    Sharemem, Windows, ExptIntf, ToolIntf;

    type
      THelpExpert =
        class( TIExpert )
          public
            function GetName: string;            override; stdcall;
            function GetAuthor: string;          override; stdcall;
            function GetComment: string;         override; stdcall;
            function GetPage: string;            override; stdcall;
            function GetGlyph: HICON;            override; stdcall;
            function GetStyle: TExpertStyle;     override; stdcall;
            function GetState: TExpertState;     override; stdcall;
            function GetIDString: string;        override; stdcall;
            function GetMenuText: string;        override; stdcall;
            procedure Execute;                   override; stdcall;
        end;

  var
    Expert: THelpExpert;

  {$R *.res}

  // THelpExpert
  function THelpExpert.GetName: string;
    begin
      result := 'Probando';
    end;

  function THelpExpert.GetAuthor: string;
    begin
      result := 'Roberto Alonso Gómez';
    end;

  function THelpExpert.GetComment: string;
    begin
      result := 'Probando hacer un experto';
    end;

  function THelpExpert.GetPage: string;
    begin
      result := 'Merchise';
    end;

  function THelpExpert.GetGlyph: HICON;
    begin
      result := 0;
    end;

  function THelpExpert.GetStyle: TExpertStyle;
    begin
      result := esForm;
    end;

  function THelpExpert.GetState: TExpertState;
    begin
      result := [esEnabled];
    end;

  function THelpExpert.GetIDString: string;
    begin
      result := 'Rag.001';
    end;

  function THelpExpert.GetMenuText: string;
    begin
      result := 'Merchise';
    end;

  procedure THelpExpert.Execute;
    begin
      // Aquí va el código del experto
    end;

  procedure DoneExpert; export;
    begin
      Expert.free;
    end;

  function InitExpert(ToolServices : TIToolServices; RegisterProc : TExpertRegisterProc; var Terminate : TExpertTerminateProc): boolean; export; stdcall;
    begin
      Expert := THelpExpert.create;
      RegisterProc(Expert);
      Terminate  := DoneExpert;
      Result := true;
    end;

  exports
    InitExpert name ExpertEntryPoint resident;
end.
