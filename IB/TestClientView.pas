unit TestClientView;

interface

  uses
    IBSystem, IBTestMain;

  type
    TTestClientView =
      class(TIBClientView)
        public
          procedure Report( aReport : string ); override;
      end;

implementation

  uses
    classes;

  // TTestClientView

  procedure TTestClientView.Report( aReport : string );
    var
      strList : TStringList;
      i       : integer;
    begin
      strList := TStringList.Create;
      try
        strList.Text := aReport;
        for i := 0 to pred(strList.Count) do
          Form1.lbOutput.Items.Add( strList[i] );

        Form1.lbOutput.Items.Add( ' ' );
        Form1.lbOutput.Items.Add( '>>>>>>>>>>>>>>>>>>>>>>>>>>' );
        Form1.lbOutput.Items.Add( ' ' );
      finally
        strList.Free;
      end;
    end;

end.
 