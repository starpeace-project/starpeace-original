unit DropTarget;

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    DragDrop;

  type
    TDragDropTarget =
      class( TComponent )
        protected
          fDropTarget    : TDropTarget;
          fTargetControl : TWinControl;

          function  GetOnFileDropped : TFileDroppedEvent;
          procedure SetOnFileDropped( Handler : TFileDroppedEvent );
          function  GetOnFileListDropped : TFileListDroppedEvent;
          procedure SetOnFileListDropped( Handler : TFileListDroppedEvent );
          function  GetAcceptOnlyOneFile : boolean;
          procedure SetAcceptOnlyOneFile( flag : boolean );

          procedure AttachTo( aControl : TWinControl );                                virtual;

        published
          property TargetControl : TWinControl               read fTargetControl       write AttachTo;
          property OnFileDropped     : TFileDroppedEvent     read GetOnFileDropped     write SetOnFileDropped;
          property OnFileListDropped : TFileListDroppedEvent read GetOnFileListDropped write SetOnFileListDropped;
          property AcceptOnlyOneFile : boolean               read GetAcceptOnlyOneFile write SetAcceptOnlyOneFile  default false;

        public
          constructor Create( anOwner : TComponent );                                  override;
          destructor  Destroy;                                                         override;
      end;

  // Component registration

  procedure Register;

implementation

  {$R *.dcr}
  
  procedure TDragDropTarget.AttachTo( aControl : TWinControl );
    var
      bakAcceptOnlyOneFile : boolean;
      bakOnFileDropped     : TFileDroppedEvent;
      bakOnFileListDropped : TFileListDroppedEvent;
    begin
      if aControl <> nil
        then fTargetControl := aControl
        else fTargetControl := TWinControl( Owner );

      bakAcceptOnlyOneFile := AcceptOnlyOneFile;
      bakOnFileDropped     := OnFileDropped;
      bakOnFileListDropped := OnFileListDropped;

      fDropTarget.Free;
      fDropTarget := TDropTarget.Create( fTargetControl.Handle );
      with fDropTarget do
        begin
          AcceptOnlyOneFile := bakAcceptOnlyOneFile;
          OnFileDropped     := bakOnFileDropped;
          OnFileListDropped := bakOnFileListDropped;
        end;
    end;

  constructor TDragDropTarget.Create( anOwner : TComponent );
    begin
      inherited;

      AttachTo( Owner as TWinControl );
    end;

  destructor TDragDropTarget.Destroy;
    begin
      fDropTarget.Free;

      inherited;
    end;

  function TDragDropTarget.GetAcceptOnlyOneFile : boolean;
    begin
      if Assigned( fDropTarget )
        then Result := fDropTarget.AcceptOnlyOneFile
        else Result := false;
    end;

  procedure TDragDropTarget.SetAcceptOnlyOneFile( flag : boolean );
    begin
      if Assigned( fDropTarget )
        then fDropTarget.AcceptOnlyOneFile := flag;
    end;

  function TDragDropTarget.GetOnFileDropped : TFileDroppedEvent;
    begin
      if Assigned( fDropTarget )
        then Result := fDropTarget.OnFileDropped
        else Result := nil;
    end;

  procedure TDragDropTarget.SetOnFileDropped( Handler : TFileDroppedEvent );
    begin
      if Assigned( fDropTarget )
        then fDropTarget.OnFileDropped := Handler;
    end;

  function TDragDropTarget.GetOnFileListDropped : TFileListDroppedEvent;
    begin
      if Assigned( fDropTarget )
        then Result := fDropTarget.OnFileListDropped
        else Result := nil;
    end;

  procedure TDragDropTarget.SetOnFileListDropped( Handler : TFileListDroppedEvent );
    begin
      if Assigned( fDropTarget )
        then fDropTarget.OnFileListDropped := Handler;
    end;

  // Component registration

  procedure Register;
    begin
      RegisterComponents( 'Merchise', [TDragDropTarget] );
    end;

end.

