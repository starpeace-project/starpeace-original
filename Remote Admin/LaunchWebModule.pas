unit LaunchWebModule;

interface

  uses
    Windows, Messages, SysUtils, Classes, HTTPApp;

  type
    TWebModule1 =
      class(TWebModule)
          procedure WebModule1WebActionItem1Action(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
        private
          { Private declarations }
        public
          { Public declarations }
      end;

  var
    WebModule1: TWebModule1;

implementation

  {$R *.DFM}

  uses
    ComObj, ShellAPI, Logs, RemoteAdm;

  procedure TWebModule1.WebModule1WebActionItem1Action(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    var
      programpath : string;
      //TM          : variant;
      procid      : integer;
      //startupinfo : TStartupInfo;
    begin
      programpath := Request.QueryFields.Values['FileName'];
      if programpath <> ''
        then
          begin
            Log('Launcher', 'File name is ' + programpath);
            {
            TM := CreateOleObject('RemoteAdmin.TaskManager');
            TM.LaunchTask(programpath);
            TM := NULL;
            }
            if RemoteAdm.StartProgram(programpath, procid)
              then Log('Launcher', 'Program launched successfully')
              else Log('Launcher', 'Unable to launch program');
          end;
      Response.StatusCode := 200;
      Response.LastModified := Now;
      Response.Expires := 0;
      Response.Date := Now;
      Handled := true;
    end;

end.
