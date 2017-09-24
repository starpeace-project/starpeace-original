unit MiscEditors;

interface

  uses
    Classes, Forms, Dialogs, ExtDlgs, DsgnIntf, TypInfo, SysUtils;

  type
    TFilenameProperty =
      class( TStringProperty )
        public
          procedure Edit;                                                                       override;
          function  GetAttributes : TPropertyAttributes;                                        override;
      end;

  procedure Register;

implementation

  uses
    CommRez;
    
  // TFilename

  procedure TFilenameProperty.Edit;
  var
    OpenDialog : TOpenDialog;
  begin
    OpenDialog := TOpenDialog.Create( Application );
    with OpenDialog do
      try
        Options    := [ofFileMustExist, ofHideReadOnly];
        Title      := SFileToLoad;
        Filter     := SAllFilesMask;
        DefaultExt := '';
        if Execute
          then SetValue( Filename );
      finally
         OpenDialog.Free;
      end;
  end;

  function TFilenameProperty.GetAttributes : TPropertyAttributes;
    begin
      Result := [paDialog, paRevertable];
    end;

  // VCL Registration

  procedure Register;
    begin
      RegisterPropertyEditor( TypeInfo( string ), nil, 'Filename', TFilenameProperty );
    end;

end.
