unit AnimationCtrl;

interface

  uses
    Windows,
    Classes, Controls;

  type
    TAnimationControl =
      class( TWinControl )
        public
          constructor Create( aOwner : TComponent );                  override;
          destructor Destroy;                                         override;
        protected
          procedure CreateParams( var Params : TCreateParams );       override;
        private
          procedure Open( aFilename : string );
          procedure Play( aFrom, aTo, aRep : integer );
          procedure Stop;
          procedure Close;
          procedure Seek( aFrame : integer );
        private
          fCentered    : boolean;
          fTransparent : boolean;
          fAutoPlay    : boolean;
          fUseTimers   : boolean;
          fFileName    : string;
        private
          procedure SetFileName( aFileName : string );
        public
          property FileName : string read fFileName write SetFileName;
      end;

implementation

  uses
    Messages, SysUtils, CommCtrl;

  const
    AnimateClass = 'SysAnimate32';

  const
    ACS_CENTER      = $0001;
    ACS_TRANSPARENT = $0002;
    ACS_AUTOPLAY    = $0004;
    ACS_TIMER       = $0008;  // don't use threads... use timers

  const
    ACM_OPENA = WM_USER + 100;
    ACM_OPENW = WM_USER + 103;
    ACM_OPEN  = ACM_OPENA;

  const
    ACM_PLAY = WM_USER + 101;
    ACM_STOP = WM_USER + 102;

  const
    ACN_START = 1;
    ACN_STOP  = 2;

  type
    EAnimControl = class(Exception);

  constructor TAnimationControl.Create( aOwner : TComponent );
    begin
      inherited;
      Width := 100;
      Height := 100;

      fCentered := true;
      fTransparent := true;
      fAutoPlay := true;
    end;

  destructor TAnimationControl.Destroy;
    begin
      inherited;
    end;

  procedure TAnimationControl.CreateParams(var Params: TCreateParams);
    begin
      InitCommonControls;
      inherited CreateParams( Params );
      CreateSubClass( Params, AnimateClass );
      with Params do
        begin
          if fCentered
            then Style := Style or ACS_CENTER;
          if fTransparent
            then Style := Style or ACS_TRANSPARENT;
          if fAutoPlay
            then Style := Style or ACS_AUTOPLAY;
          if fUseTimers
            then Style := Style or ACS_TIMER;
        end;
    end;

  procedure TAnimationControl.Open( aFilename : string );
    begin
      if SendMessage( Handle, ACM_OPEN, 0, longint(pchar(aFileName)) ) = 0
        then raise EAnimControl.Create( 'Cannot open animation' );
    end;

  procedure TAnimationControl.Play( aFrom, aTo, aRep : integer );
    begin
      if SendMessage( Handle, ACM_PLAY, aRep, MakeLong(aFrom, aTo) ) = 0
        then raise EAnimControl.Create( 'Cannot play animation' );
    end;

  procedure TAnimationControl.Stop;
    begin
      if SendMessage( Handle, ACM_STOP, 0, 0 ) = 0
        then raise EAnimControl.Create( 'Cannot stop animation' );
    end;

  procedure TAnimationControl.Close;
    begin
      if SendMessage( Handle, ACM_OPEN, 0, 0 ) = 0
        then raise EAnimControl.Create( 'Cannot close animation' );
    end;

  procedure TAnimationControl.Seek( aFrame : integer );
    begin
      Play( aFrame, aFrame, 1 );
    end;

  procedure TAnimationControl.SetFileName( aFileName : string );
    begin
      fFileName := aFileName;
      Open( fFileName );
    end;
    
end.
