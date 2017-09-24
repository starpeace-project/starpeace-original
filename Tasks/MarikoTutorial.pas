unit MarikoTutorial;

interface

  uses
    SysUtils, Tasks;

  const
    tidTask_ClusterMariko             = 'MarikoTutorial';
    tidTask_Mariko_MainHeadquarter    = 'MarikoMainHq';
    tidTask_Mariko_SelectProduct      = 'MarikoSelProduct';
    tidTask_Mariko_PharmaTutorial     = 'MarikoPharmaTutorial';

  procedure RegisterTasks;

implementation

  uses
    Kernel, ClusterTask, HeadquarterTasks, FoodStore, Tutorial, CommonTasks,
    BasicAccounts, StdAccounts, StdFluids, FacIds;

  // Register Mariko Tutorial Tasks

  procedure RegisterTasks;
    begin
      with TMetaClusterTask.Create(
        tidTask_ClusterMariko,
        'Mariko Cluster Tutorial',
        '',   // No description
        tidTask_Tutorial,   // None
        10,
        TClusterTask) do
        begin
          Priority := tprHighest - 1;
          ClusterName := 'Mariko';
          Register(tidClassFamily_Tasks);
        end;

      {// Main Headquarter
      with TMetaHeadquarterTask.Create(
        tidTask_MainHeadquarter,
        tidTask_Mariko_MainHeadquarter,
        tidTask_ClusterMariko) do
        begin
          Priority := tprNormal - 1;
          Register(tidClassFamily_Tasks);
        end;}

      // Select Product Task
      with TMetaTask.Create(
        tidTask_SelectProduct,
        tidTask_Mariko_SelectProduct,
        tidTask_ClusterMariko) do
        begin
          Priority := tprNormal - 2;
          Register(tidClassFamily_Tasks);
        end;

      CommonTasks.AssembleTutorialBranch(
        'CDs',
        tidTask_Mariko_SelectProduct,
        'Mariko',
        tidTask_BuildGenWarehouse,
        tidFluid_CDs,
        tidTask_BuildCDStore,
        'CDInd',
        tidTask_BuildCDPlant,
        tidTask_Mariko_MainHeadquarter,
        tidTask_ClusterMariko,
        accIdx_CDStore,
        FID_CDStore,
        FID_CDPlant,
        0.667);

      CommonTasks.AssembleTutorialBranch(
        'HHA',
        tidTask_Mariko_SelectProduct,
        'Mariko',
        tidTask_BuildGenWarehouse,
        tidFluid_HouseHoldingAppliances,
        tidTask_BuildHHAStore,
        'HHALic',
        tidTask_BuildHHAInd,
        tidTask_Mariko_MainHeadquarter,
        tidTask_ClusterMariko,
        accIdx_HHAStore,
        FID_HHAStore,
        FID_Household,
        0.1665);

      CommonTasks.AssembleTutorialBranch(
        'Clothes',
        tidTask_Mariko_SelectProduct,
        'Mariko',
        tidTask_BuildGenWarehouse,
        tidFluid_Clothes,
        tidTask_BuildClotheStore,
        'ClothesLic',
        tidTask_BuildClotheInd,
        tidTask_Mariko_MainHeadquarter,
        tidTask_ClusterMariko,
        accIdx_ClothesStore,
        FID_ClotheStore,
        FID_Clothes,
        0.1665);

    end;

end.

