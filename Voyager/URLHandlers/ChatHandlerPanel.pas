unit ChatHandlerPanel;

interface

  uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    ExtCtrls, StdCtrls, Protocol;

  type
    TOnMessageComposed           = procedure( Msg : string ) of object;
    TOnMessageCompositionChanged = procedure( State : TMsgCompositionState ) of object;

  type
    TChatHandlerPanel =
      class( TCustomPanel )
        public
          constructor Create( anOwner : TComponent ); override;
        private
          fChatLines : TMemo;
          fTextInput : TMemo;
          fSplitter  : TSplitter;
        public
          procedure DisplayMsg( From, Msg : string );
        private
          fOnMessageComposed : TOnMessageComposed;
          fOnMessageCompositionChanged : TOnMessageCompositionChanged;
        public
          property OnMessageComposed : TOnMessageComposed write fOnMessageComposed;
          property OnMessageCompositionChanged : TOnMessageCompositionChanged write fOnMessageCompositionChanged;
        private
          procedure OnKeyUp( Sender: TObject; var Key: Word; Shift : TShiftState );
      end;

implementation

  // TChatHandlerPanel

  constructor TChatHandlerPanel.Create( anOwner : TComponent );
    const
      BackgroundColor = $99BBBB;
    var
      Panel : TPanel;
    begin
      inherited;
      BevelInner  := bvNone;
      BevelOuter  := bvNone;
      Color       := BackgroundColor;
      BorderWidth := 0;

      // Create TextInput memo
      fTextInput := TMemo.Create( self );
      fTextInput.BorderStyle := bsNone;
      fTextInput.Color       := $BBDDDD;
      fTextInput.ParentCtl3D := false;
      fTextInput.Ctl3D       := false;
      fTextInput.Font.Name   := 'Verdana';
      fTextInput.Font.Size   := 10;
      fTextInput.Height      := 60;
      fTextInput.Align       := alBottom;
      fTextInput.Alignment   := taCenter;
      fTextInput.OnKeyUp     := OnKeyUp;
      InsertControl( fTextInput );

      // Crete Splitter
      fSplitter := TSplitter.Create( self );
      fSplitter.Beveled := false;
      fSplitter.Color   := BackgroundColor;
      fSplitter.Height  := 3;
      fSplitter.Align   := alBottom;
      fSplitter.Cursor  := crHSplit;
      InsertControl( fSplitter );

      // Create ChatLines border panel
      Panel             := TPanel.Create( self );
      Panel.Align       := alClient;
      Panel.BevelInner  := bvNone;
      Panel.BevelOuter  := bvNone;
      Panel.Color       := BackgroundColor;
      Panel.BorderWidth := 7;
      InsertControl( Panel );

      // Create ChatLines memo
      fChatLines  := TMemo.Create( self );
      fChatLines.BorderStyle := bsNone;
      fChatLines.Color       := BackgroundColor;
      fChatLines.Font.Name   := 'Verdana';
      fChatLines.Font.Size   := 10;
      fChatLines.Height      := 3;
      fChatLines.Align       := alClient;
      Panel.InsertControl( fChatLines );
    end;

  procedure TChatHandlerPanel.DisplayMsg( From, Msg : string );
    begin
      fChatLines.Lines.Add( From + ': ' + Msg );
    end;

  procedure TChatHandlerPanel.OnKeyUp( Sender: TObject; var Key: Word; Shift : TShiftState );
    var
      text : string;
      p    : integer;
    begin
      if Key = vk_Return
        then
          begin
            text := fTextInput.Text;
            p := pos( #$D#$A, text );
            if p > 0
              then delete( text, p, 2 );
            if assigned(fOnMessageComposed)
              then fOnMessageComposed( text );
            fTextInput.Clear;
          end;
    end;

end.
