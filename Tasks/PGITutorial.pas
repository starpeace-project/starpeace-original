unit PGITutorial;

interface

  uses
    SysUtils, Tasks;

  const
    tidTask_ClusterPGI             = 'PGITutorial';
    tidTask_PGI_MainHeadquarter    = 'PGIMainHq';
    tidTask_PGI_SelectProduct      = 'PGISelProduct';
    tidTask_PGI_PharmaTutorial     = 'PGIPharmaTutorial';
    //tidTask_PGI_GenBuildIWarehouse = 'PGIBuildGenWH';
    //tidTask_PGI_GenBuildEWarehouse = 'PGIBuildGenWHExp';
    //tidTask_PGI_CommHeadquarter    = 'PGICommHq';
    //tidTask_PGI_IndHeadquarter     = 'PGIIndHq';
    //tidTask_PGI_BuildMart          = 'PGIBuildMart';
    //tidTask_PGI_BuildPharmacy      = 'PGIBuildPharmacy';
    //tidTask_PGI_MakeProfit         = 'PGIMakeProfit';
    //tidTask_PGI_ResearchSomething  = 'PGIResSomething';
    //tidTask_PGI_BuildPharmaInd     = 'PGIPharmaInd';

  procedure RegisterTasks;

implementation

  uses
    Kernel, ClusterTask, HeadquarterTasks, FoodStore, Tutorial, CommonTasks,
    BasicAccounts, StdAccounts, StdFluids, FacIds;

  // Register PGI Tutorial Tasks

  procedure RegisterTasks;
    begin
      with TMetaClusterTask.Create(
        tidTask_ClusterPGI,
        'PGI Cluster Tutorial',
        '',   // No description
        tidTask_Tutorial,   // None
        10,
        TClusterTask) do
        begin
          Priority := tprHighest - 1;
          ClusterName := 'PGI';
          Register(tidClassFamily_Tasks);
        end;

      {// Main Headquarter
      with TMetaHeadquarterTask.Create(
        tidTask_MainHeadquarter,
        tidTask_PGI_MainHeadquarter,
        tidTask_ClusterPGI) do
        begin
          Register(tidClassFamily_Tasks);
        end;}

      // Select Product Task
      with TMetaTask.Create(
        tidTask_SelectProduct,
        tidTask_PGI_SelectProduct,
        tidTask_ClusterPGI) do
        begin
          Register(tidClassFamily_Tasks);
        end;

      CommonTasks.AssembleTutorialBranch(
        'Pharma',
        tidTask_PGI_SelectProduct,
        'PGI',
        tidTask_BuildGenWarehouse,
        tidGate_Drugs,
        tidTask_BuildPharmacy,
        'PharmaIndustry',
        tidTask_BuildPharmaInd,
        tidTask_PGI_MainHeadquarter,
        tidTask_ClusterPGI,
        accIdx_DrugStore,
        FID_DrugStore,
        FID_Pharmaceutics,
        1);

    end;

end.

