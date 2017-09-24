object ServerReportWebModule: TServerReportWebModule
  OldCreateOrder = False
  Actions = <
    item
      Default = True
      MethodType = mtGet
      Name = 'GetReport'
      OnAction = ServerReportWebModuleGetReportAction
    end>
  Left = 204
  Top = 179
  Height = 480
  Width = 696
end
