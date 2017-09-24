unit MoabTutorial;

interface

  uses
    SysUtils, Tasks;

  const
    tidTask_ClusterMoab             = 'MoabTutorial';
    tidTask_Moab_MainHeadquarter    = 'MoabMainHq';
    tidTask_Moab_SelectProduct      = 'MoabSelProduct';
    tidTask_Moab_PharmaTutorial     = 'MoabPharmaTutorial';

  procedure RegisterTasks;

implementation

  uses
    Kernel, ClusterTask, HeadquarterTasks, FoodStore, Tutorial, CommonTasks,
    BasicAccounts, StdAccounts, StdFluids, FacIds;

  // Register Moab Tutorial Tasks

  procedure RegisterTasks;
    begin
      with TMetaClusterTask.Create(
        tidTask_ClusterMoab,
        'Moab Cluster Tutorial',
        '',   // No description
        tidTask_Tutorial,   // None
        10,
        TClusterTask) do
        begin
          Priority := tprHighest - 1;
          ClusterName := 'Moab';
          Register(tidClassFamily_Tasks);
        end;

      {// Main Headquarter
      with TMetaHeadquarterTask.Create(
        tidTask_MainHeadquarter,
        tidTask_Moab_MainHeadquarter,
        tidTask_ClusterMoab) do
        begin
          Priority := tprNormal - 1;
          Register(tidClassFamily_Tasks);
        end;}

      // Select Product Task
      with TMetaTask.Create(
        tidTask_SelectProduct,
        tidTask_Moab_SelectProduct,
        tidTask_ClusterMoab) do
        begin
          Priority := tprNormal - 2;
          Register(tidClassFamily_Tasks);
        end;

      CommonTasks.AssembleTutorialBranch(
        'Gas',
        tidTask_Moab_SelectProduct,
        'Moab',
        tidTask_BuildGenWarehouse,
        tidFluid_Gasoline,
        tidTask_BuildGasStation,
        'Refinery',
        tidTask_BuildRefinery,
        tidTask_Moab_MainHeadquarter,
        tidTask_ClusterMoab,
        accIdx_GasStation,
        FID_GasStation,
        FID_Refinery,
        0.667);

      CommonTasks.AssembleTutorialBranch(
        'Clothes',
        tidTask_Moab_SelectProduct,
        'Moab',
        tidTask_BuildGenWarehouse,
        tidFluid_Clothes,
        tidTask_BuildClotheStore,
        'ClothesLic',
        tidTask_BuildClotheInd,
        tidTask_Moab_MainHeadquarter,
        tidTask_ClusterMoab,
        accIdx_ClothesStore,
        FID_ClotheStore,
        FID_Clothes,
        0.1665);

      CommonTasks.AssembleTutorialBranch(
        'HHA',
        tidTask_Moab_SelectProduct,
        'Moab',
        tidTask_BuildGenWarehouse,
        tidFluid_HouseHoldingAppliances,
        tidTask_BuildHHAStore,
        'HHALic',
        tidTask_BuildHHAInd,
        tidTask_Moab_MainHeadquarter,
        tidTask_ClusterMoab,
        accIdx_HHAStore,
        FID_HHAStore,
        FID_Household,
        0.1665);

    end;

end.

