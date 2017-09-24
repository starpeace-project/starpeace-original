unit VisualControls;

interface

  uses
    Classes, Controls, Windows, SysUtils, Messages, ExtCtrls;

  //TVisualControl must be the ancestor of  any form that pretending to use as a control,
  //setting IsControl property to true (Viewing a Form as Text).
  //See TControl.IsControl property in Delphi help.

        //It is necessary to declare PixelsInch and TextHeight variables because
        //Delphi's environment save these variables as they were Control's variables
        //instead of Form's variables. So it is necessary to declare these two variables
        //in order to load the correct Form(Control in this case) data from the .dfm file.

        //AlignedControl property specify the control that is aligned in the client area.
        //Assigning a value to AlignedControl align that control in the client area
        //according to the value of the ControlAligmnet property. Note that the control
        //specified AlignedControl property must be a child of the control.
        
  type
    TControlAlignment =  (caBottom, caBottomCenter, caBottomLeft, caBottomRight, caCenter,
                          caLeft, caLeftBottom, caLeftCenter, caLeftTop, caNone, caDefault,
                          caRight, caRightBottom, caRightCenter, caRightTop,
                          caTop, caTopCenter, caTopLeft, caTopRight );

    TAligningControlEvent = procedure(Control : Tcontrol; Alignment : TControlAlignment) of object;
  type
    TVisualControl =
      class(TPanel)
        private
          procedure WMSize(var Message);                                message WM_SIZE;
        protected
          procedure Loaded;                                             override;
        public
          constructor Create(aOwner : TComponent);                      override;
          destructor  Destroy;                                          override;
        published
          property Align           default alClient;
          property DragCursor;
          property DragMode;
          property Enabled;
          property Caption;
          property ClientHeight;
          property ClientWidth;
          property Color;
          property Ctl3D;
          property Font;
          property ParentColor     default true;
          property ParentCtl3D;
          property ParentFont;
          property ParentShowHint;
          property PopupMenu;
          property ShowHint;
          property TabOrder;
          property TabStop;
          property Visible;
          property OnClick;
          property OnDblClick;
          property OnDragDrop;
          property OnDragOver;
          property OnEndDrag;
          property OnEnter;
          property OnExit;
          property OnMouseDown;
          property OnMouseMove;
          property OnMouseUp;
          property OnStartDrag;
        private
          PPI : integer;
          TH  : integer;
          fOldCreateOrder: boolean;
        published
          property PixelsPerInch : integer    read PPI              write PPI;
          property TextHeight    : integer    read TH               write TH;
          property OldCreateOrder: boolean    read fOldCreateOrder  write fOldCreateOrder;       
        private
          fDACPos           : TRect;
          fControlAlignment : TControlAlignment;
          fAlignedControl   : TControl;
          fOnAlgnCtrl       : TAligningControlEvent;
          procedure AlignControl;
          procedure SetAlignedControl(value : TControl);
          procedure SetControlAlignment(value : TControlAlignment);
          function  IsReallyShowing : boolean;
        public
          property AlignedControl    : TControl              read fAlignedControl   write SetAlignedControl;
          property ControlAlignment  : TControlAlignment     read fControlAlignment write SetControlAlignment default caCenter;
          property OnAligningControl : TAligningControlEvent read fOnAlgnCtrl       write fOnAlgnCtrl;
          property DefaultAlignedControlPosition : TRect     read fDACPos;
          property ReallyShowing : boolean                   read IsReallyShowing;
        protected
          procedure SetParent(which : TWinControl);  override;  // ><
      end;

  type
    EVisualControlError = class (Exception);

implementation

  uses
    Forms;

  { TVisualControl }

  constructor TVisualControl.Create(aOwner : TComponent);  // ><
    begin
      inherited;
      //Align := alClient;
      fAlignedControl := nil;
      fControlAlignment := caCenter;
    end;

  destructor TVisualControl.Destroy;
    begin
      Destroying;
      RemoveFixupReferences(Self, '');
      inherited;
    end;

  procedure TVisualControl.Loaded;
    begin
      inherited;
      {
      if not (csDesigning in ComponentState)
        then Name := '';
      for i := 0 to pred(ComponentCount) do
        Components[i].Name := '';
      }
      Caption := '';
      ParentColor := true;
      BevelInner := bvNone; 
      BevelOuter := bvNone; 
    end;

  procedure TVisualControl.WMSize(var Message);
    begin
      inherited;
      //AlignControl;
    end;

  procedure TVisualControl.AlignControl;
    begin
      if (fAlignedControl <> nil) and (fAlignedControl.Align = alNone)
        then
          if Assigned(fOnAlgnCtrl)
            then
              fOnAlgnCtrl(fAlignedControl, fControlAlignment)
            else
              with fAlignedControl do
                case fControlAlignment of
                  caBottom       :
                    begin
                      Top  := (Self.Height - Height);
                    end;
                  caBottomCenter :
                    begin
                      Left := (Self.Width div 2) - (Width div 2);
                      Top  := (Self.Height - Height);
                    end;
                  caBottomLeft, caLeftBottom   :
                    begin
                      Left := 0;
                      Top  := (Self.Height - Height);
                    end;
                  caBottomRight, caRightBottom :
                    begin
                      Left := (Self.Width - Width);
                      Top  := (Self.Height - Height);
                    end;
                  caCenter       :
                    begin
                      Left := (Self.Width div 2) - (Width div 2);
                      Top  := (Self.Height div 2) - (Height div 2);
                    end;
                  caLeft          :
                    begin
                      Left := 0;
                    end;
                  caLeftCenter    :
                    begin
                      Left := 0;
                      Top  := (Self.Height div 2) - (Height div 2);
                    end;
                  caLeftTop, caTopLeft         :
                    begin
                      Left := 0;
                      Top  := 0;
                    end;
                  caDefault       :
                    begin
                      Left := fDACPos.Left;
                      Top  := fDACPos.Top;
                    end;
                  caRight         :
                    begin
                      Left := (Self.Width - Width);
                    end;
                  caRightCenter   :
                    begin
                      Left := (Self.Width - Width);
                      Top  := (Self.Height div 2) - (Height div 2);
                    end;
                  caRightTop, caTopRight       :
                    begin
                      Left := (Self.Width - Width);
                      Top  := 0;
                    end;
                  caTop           :
                    begin
                      Top := 0;
                    end;
                  caTopCenter     :
                    begin
                      Left := (Self.Width div 2) - (Width div 2);
                      Top := 0;
                    end;
                end;
    end;

  procedure TVisualControl.SetAlignedControl(value : TControl);
    begin
      fAlignedControl := value;
      if value <> nil
        then
          begin
            fDACPos := value.BoundsRect;
            AlignControl;
          end
        else
          fDACPos := Rect(0, 0, 0, 0);
    end;

  procedure TVisualControl.SetControlAlignment(value : TControlAlignment);
    begin
      fControlAlignment := value;
      AlignControl;
    end;

  function TVisualControl.IsReallyShowing : boolean;

    function OneIsHidden( Control : TControl ) : boolean;
      begin
        result := not Control.Visible or ((Control.Parent <> nil) and OneIsHidden( Control.Parent ));
      end;

    begin
      result := not OneIsHidden( self );
    end;

  procedure TVisualControl.SetParent(which : TWinControl);  // ><
    begin
      inherited;
      if Parent <> nil
        then InitInheritedComponent(Self, TCustomControl);
    end;


end.
