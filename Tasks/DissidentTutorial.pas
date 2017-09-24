unit DissidentTutorial;

interface

  uses
    SysUtils, Tasks;

  const
    tidTask_ClusterDiss             = 'DissTutorial';
    tidTask_Diss_MainHeadquarter    = 'DissMainHq';
    tidTask_Diss_SelectProduct      = 'DissSelProduct';
    tidTask_Diss_PharmaTutorial     = 'DissPharmaTutorial';

  procedure RegisterTasks;

implementation

  uses
    Kernel, ClusterTask, HeadquarterTasks, FoodStore, Tutorial, CommonTasks,
    BasicAccounts, StdAccounts, StdFluids, FacIds;

  // Register Diss Tutorial Tasks

  procedure RegisterTasks;
    begin
      with TMetaClusterTask.Create(
        tidTask_ClusterDiss,
        'Dissidents Cluster Tutorial',
        '',   // No description
        tidTask_Tutorial,   // None
        10,
        TClusterTask) do
        begin
          Priority := tprHighest - 1;
          ClusterName := 'Dissidents';
          Register(tidClassFamily_Tasks);
        end;

      {// Main Headquarter
      with TMetaHeadquarterTask.Create(
        tidTask_MainHeadquarter,
        tidTask_Diss_MainHeadquarter,
        tidTask_ClusterDiss) do
        begin
          Priority := tprNormal;
          Register(tidClassFamily_Tasks);
        end;}

      // Select Product Task
      with TMetaTask.Create(
        tidTask_SelectProduct,
        tidTask_Diss_SelectProduct,
        tidTask_ClusterDiss) do
        begin
          Priority := tprNormal - 1;
          Register(tidClassFamily_Tasks);
        end;

      CommonTasks.AssembleTutorialBranch(
        'Toys',
        tidTask_Diss_SelectProduct,
        'Diss',
        tidTask_BuildGenWarehouse,
        tidGate_Toys,
        tidTask_BuildToyStore,
        'ToyIndustry',
        tidTask_BuildToyInd,
        tidTask_Diss_MainHeadquarter,
        tidTask_ClusterDiss,
        accIdx_ToyStore,
        FID_ToyStore,
        FID_Toys,
        0.334);

      CommonTasks.AssembleTutorialBranch(
        'HHA',
        tidTask_Diss_SelectProduct,
        'Diss',
        tidTask_BuildGenWarehouse,
        tidGate_HouseHoldingAppliances,
        tidTask_BuildHHAStore,
        'HHALic',
        tidTask_BuildHHAInd,
        tidTask_Diss_MainHeadquarter,
        tidTask_ClusterDiss,
        accIdx_HHAStore,
        FID_HHAStore,
        FID_Household,
        0.1663);

      CommonTasks.AssembleTutorialBranch(
        'Furnitures',
        tidTask_Diss_SelectProduct,
        'Diss',
        tidTask_BuildGenWarehouse,
        tidGate_Furniture,
        tidTask_BuildFurnitureStore,
        'FurnLic',
        tidTask_BuildFurnitureInd,
        tidTask_Diss_MainHeadquarter,
        tidTask_ClusterDiss,
        accIdx_FurnitureStore,
        fid_furniture,
        FID_FurnitureInd,
        0.334);

      CommonTasks.AssembleTutorialBranch(
        'Clothes',
        tidTask_Diss_SelectProduct,
        'Diss',
        tidTask_BuildGenWarehouse,
        tidGate_Clothes,
        tidTask_BuildClotheStore,
        'ClothesLic',
        tidTask_BuildClotheInd,
        tidTask_Diss_MainHeadquarter,
        tidTask_ClusterDiss,
        accIdx_ClothesStore,
        FID_ClotheStore,
        FID_Clothes,
        0.1663);

    end;

end.

