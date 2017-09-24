unit StatsExplorerPlotter;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, TeeProcs, TeEngine, Chart, Series, StdCtrls, ComCtrls;


type
  TStatsForm = class(TForm)
    Chart1: TChart;
    Panel1: TPanel;
    Button1: TButton;
    ProgressBar1: TProgressBar;
    CheckBox1: TCheckBox;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  StatsForm: TStatsForm;

implementation

  uses
    StrUtils, Explorer;

{$R *.DFM}

procedure TStatsForm.Button1Click(Sender: TObject);
    var
{     DSCnx     : IRDOConnectionInit;
      WSDSCnx   : TWinSockRDOConnection;
      DSProxy   : OleVariant;}
      SessionId : integer;
      keys      : TStringList;
      values    : TStringList;
      i, j, val : integer;
      lastval   : integer;
      L         : TLineSeries;
      date      : TDateTime;
    begin
      Chart1.RemoveAllSeries;
      try
        {WSDSCnx      := TWinSockRDOConnection.Create( 'CNX1' );
        DSCnx        := WSDSCnx;
        DSCnx.Server := Edit1.Text;
        DSCnx.Port   := StrToInt(Edit2.Text);
        DSProxy      := TRDOObjectProxy.Create as IDispatch;}
        {if DSCnx.Connect( 10000 )
          then}
            try
              {DSProxy.SetConnection( DSCnx );
              DSProxy.BindTo( 'DirectoryServer' );
              DSProxy.WaitForAnswer := true;
              SessionId := DSProxy.RDOOpenSession;}
              {if SessionId <> 0
                then}
                  begin
                    //SQLExplorer.ISProxy.BindTo( SessionId );
                    try
                      if SQLExplorer.ISProxy.RDOSetCurrentKey( 'Root/Stats' )
                        then
                          begin
                            keys := TStringList.Create;
                            keys.text := SQLExplorer.ISProxy.RDOGetKeyNames;
                            values := TStringList.Create;
                            values.text := SQLExplorer.ISProxy.RDOGetValueNames;
                            ProgressBar1.Max := keys.count*values.count;
                            ProgressBar1.Position := 0;
                            for j := 0 to pred(values.count) do
                              begin
                                if SQLExplorer.Ending then
                                   exit ;
                                L := TLineSeries.Create( nil );
                                L.Name := values[j];
                                L.XValues.DateTime := true;
                                lastval := -1;
                                for i := 0 to pred(keys.count) do
                                  begin
                                    if SQLExplorer.Ending then
                                      exit ;
                                    if SQLExplorer.ISProxy.RDOSetCurrentKey( 'Root/Stats/' + keys[i] )
                                      then
                                        begin
                                          date := StrToDate(ReplaceChar(keys[i], '-', '/' ));
                                          val := SQLExplorer.ISProxy.RDOReadInteger( values[j] );
                                          if lastval = -1
                                            then lastval := val;
                                          if not CheckBox1.Checked
                                            then L.AddXY( date, val )
                                            else L.AddXY( date, val - lastval );
                                          lastval := val;
                                          ProgressBar1.Position := ProgressBar1.Position + 1;
                                          Application.ProcessMessages;
                                        end;
                                  end;
                                Chart1.AddSeries( L );
                              end;
                          end;
                      ProgressBar1.Position := 0;
                    finally
                      {DSProxy.RDOEndSession;}
                    end;
                  end;
            finally
              {DSCnx.Disconnect;}
            end;
      except
      end;
    end;

end.
