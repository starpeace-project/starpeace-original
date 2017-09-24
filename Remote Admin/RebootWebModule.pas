unit RebootWebModule;

interface

  uses
    Windows, Messages, SysUtils, Classes, HTTPApp;

  type
    TRebWebModule =
      class(TWebModule)
          procedure RebWebModuleWebActionItem1Action(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
        private
          { Private declarations }
        public
          { Public declarations }
      end;

  var
    RebWebModule: TRebWebModule;

implementation

  {$R *.DFM}

  uses
    ComObj;

  procedure TRebWebModule.RebWebModuleWebActionItem1Action(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    var
      TM : variant;
    begin
      TM := CreateOLEObject('RemoteAdmin.TaskManager');
      TM.Reboot;
      TM := NULL;
      Response.StatusCode := 200;
      Response.LastModified := Now;
      Response.Expires := 0;
      Response.Date := Now;
      Handled := true;
    end;

end.
