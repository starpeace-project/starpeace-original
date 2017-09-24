unit KernelBackup;

interface

  uses
    BackupObjects;

  type
    TFacilityBackupAgent =
      class(TBackupAgent)
        protected
          class procedure Write(Stream : TBackupWriter; Obj : TObject); override;
          class procedure Read (Stream : TBackupReader; Obj : TObject); override;
      end;

    TBlockBackupAgent =
      class(TBackupAgent)
        protected
          class procedure Write(Stream : TBackupWriter; Obj : TObject); override;
          class procedure Read (Stream : TBackupReader; Obj : TObject); override;
      end;

    TGateBackupAgent =
      class(TBackupAgent)
        protected
          class procedure Write(Stream : TBackupWriter; Obj : TObject); override;
          class procedure Read (Stream : TBackupReader; Obj : TObject); override;
      end;

    TInputBackupAgent =
      class(TBackupAgent)
        protected
          class procedure Write(Stream : TBackupWriter; Obj : TObject); override;
          class procedure Read (Stream : TBackupReader; Obj : TObject); override;
      end;

    TOutputBackupAgent =
      class(TBackupAgent)
        protected
          class procedure Write(Stream : TBackupWriter; Obj : TObject); override;
          class procedure Read (Stream : TBackupReader; Obj : TObject); override;
      end;

    TTaxInfoBackupAgent =
      class(TBackupAgent)
        protected
          class procedure Write(Stream : TBackupWriter; Obj : TObject); override;
          class procedure Read (Stream : TBackupReader; Obj : TObject); override;
      end;

    TClusterBackupAgent =
      class(TBackupAgent)
        protected
          class procedure Write(Stream : TBackupWriter; Obj : TObject); override;
          class procedure Read (Stream : TBackupReader; Obj : TObject); override;
      end;

    TCompanyBackupAgent =
      class(TBackupAgent)
        protected
          class procedure Write(Stream : TBackupWriter; Obj : TObject); override;
          class procedure Read (Stream : TBackupReader; Obj : TObject); override;
      end;

    TTownBackupAgent =
      class(TBackupAgent)
        protected
          class procedure Write(Stream : TBackupWriter; Obj : TObject); override;
          class procedure Read (Stream : TBackupReader; Obj : TObject); override;
      end;

    TTycoonBackupAgent  =
      class(TBackupAgent)
        protected
          class procedure Write(Stream : TBackupWriter; Obj : TObject); override;
          class procedure Read (Stream : TBackupReader; Obj : TObject); override;
      end;

    TWorldBackupAgent  =
      class(TBackupAgent)
        protected
          class procedure Write(Stream : TBackupWriter; Obj : TObject); override;
          class procedure Read (Stream : TBackupReader; Obj : TObject); override;
      end;

  procedure RegisterBackup;

implementation

  uses
    Kernel, World;


  // TFacilityBackupAgent

  class procedure TFacilityBackupAgent.Write(Stream : TBackupWriter; Obj : TObject);
    begin
      TFacility(Obj).StoreToBackup(Stream);
    end;

  class procedure TFacilityBackupAgent.Read(Stream : TBackupReader; Obj : TObject);
    begin
      TFacility(Obj).LoadFromBackup(Stream);
    end;


  // TBlockBackupAgent

  class procedure TBlockBackupAgent.Write(Stream : TBackupWriter; Obj : TObject);
    begin
      TBlock(Obj).StoreToBackup(Stream);
    end;

  class procedure TBlockBackupAgent.Read(Stream : TBackupReader; Obj : TObject);
    begin
      TBlock(Obj).LoadFromBackup(Stream);
    end;


  // TGateBackupAgent

  class procedure TGateBackupAgent.Write(Stream : TBackupWriter; Obj : TObject);
    begin
      TGate(Obj).StoreToBackup(Stream);
    end;

  class procedure TGateBackupAgent.Read(Stream : TBackupReader; Obj : TObject);
    begin
      TGate(Obj).LoadFromBackup(Stream);
    end;


  // TInputBackupAgent

  class procedure TInputBackupAgent.Write(Stream : TBackupWriter; Obj : TObject);
    begin
      TInput(Obj).StoreToBackup(Stream);
    end;

  class procedure TInputBackupAgent.Read(Stream : TBackupReader; Obj : TObject);
    begin
      TInput(Obj).LoadFromBackup(Stream);
    end;


  // TOutputBackupAgent

  class procedure TOutputBackupAgent.Write(Stream : TBackupWriter; Obj : TObject);
    begin
      TOutput(Obj).StoreToBackup(Stream);
    end;

  class procedure TOutputBackupAgent.Read(Stream : TBackupReader; Obj : TObject);
    begin
      TOutput(Obj).LoadFromBackup(Stream);
    end;


  // TTaxInfoBackupAgent

  class procedure TTaxInfoBackupAgent.Write(Stream : TBackupWriter; Obj : TObject);
    begin
      TTaxInfo(Obj).StoreToBackup(Stream);
    end;

  class procedure TTaxInfoBackupAgent.Read(Stream : TBackupReader; Obj : TObject);
    begin
      TTaxInfo(Obj).LoadFromBackup(Stream);
    end;


  // TClusterBackupAgent

  class procedure TClusterBackupAgent.Write(Stream : TBackupWriter; Obj : TObject);
    begin
      TCluster(Obj).StoreToBackup(Stream);
    end;

  class procedure TClusterBackupAgent.Read(Stream : TBackupReader; Obj : TObject);
    begin
      TCluster(Obj).LoadFromBackup(Stream);
    end;


  // TCompanyBackupAgent

  class procedure TCompanyBackupAgent.Write(Stream : TBackupWriter; Obj : TObject);
    begin
      TCompany(Obj).StoreToBackup(Stream);
    end;

  class procedure TCompanyBackupAgent.Read(Stream : TBackupReader; Obj : TObject);
    begin
      TCompany(Obj).LoadFromBackup(Stream);
    end;


  // TTownBackupAgent

  class procedure TTownBackupAgent.Write(Stream : TBackupWriter; Obj : TObject);
    begin
      TTown(Obj).StoreToBackup(Stream);
    end;

  class procedure TTownBackupAgent.Read(Stream : TBackupReader; Obj : TObject);
    begin
      TTown(Obj).LoadFromBackup(Stream);
    end;


  // TTycoonBackupAgent

  class procedure TTycoonBackupAgent.Write(Stream : TBackupWriter; Obj : TObject);
    begin
      TTycoon(Obj).StoreToBackup(Stream);
    end;

  class procedure TTycoonBackupAgent.Read(Stream : TBackupReader; Obj : TObject);
    begin
      TTycoon(Obj).LoadFromBackup(Stream);
    end;


  // TWorldBackupAgent

  class procedure TWorldBackupAgent.Write(Stream : TBackupWriter; Obj : TObject);
    begin
      TWorld(Obj).StoreToBackup(Stream);
    end;

  class procedure TWorldBackupAgent.Read(Stream : TBackupReader; Obj : TObject);
    begin
      TWorld(Obj).LoadFromBackup(Stream);
    end;


  // Registration

  procedure RegisterBackup;
    begin
      TFacilityBackupAgent.Register([TFacility]);
      TBlockBackupAgent.Register([TBlock]);
      TGateBackupAgent.Register([TGate]);
      TInputBackupAgent.Register([TInput]);
      TOutputBackupAgent.Register([TOutput]);
      TTaxInfoBackupAgent.Register([TTaxInfo]);

      RegisterClass(TPushInput);
      RegisterClass(TPullInput);
      RegisterClass(TPushOutput);
      RegisterClass(TPullOutput);

      TClusterBackupAgent.Register([TCluster]);
      TCompanyBackupAgent.Register([TCompany]);
      TTownBackupAgent.Register([TTown]);
      TTycoonBackupAgent.Register([TTycoon]);
      TWorldBackupAgent.Register([TWorld]);
    end;

    
end.
