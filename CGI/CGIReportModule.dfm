object CGIWebModule: TCGIWebModule
  OldCreateOrder = False
  Actions = <
    item
      MethodType = mtGet
      Name = 'GetReport'
      OnAction = CGIWebModuleGetReportAction
    end>
  Left = 303
  Top = 230
  Height = 480
  Width = 696
end
