unit Voyager;

interface

  uses
    VoyagerInterfaces, Collection, Controls;

  type
    // TMetaFrame is a wrapper to a IMetaURLHandler. It contains an
    // IMetaURLHandler and also implements it.

    TMetaFrame =
      class( TObject, IURLHandler )
        public
          constructor Create( aMetaURLHandler : IMetaURLHandler );
          destructor  Destroy; override;
        private
          fMetaURLHandler : IMetaURLHandler;
          fFrames         : TCollection;
        private
          function GetName          : string;
          function GetOptions       : TURLHandlerOptions;
          function GetInstanceLimit : integer;
        public
          property Name          : string             read GetName;
          property Options       : TURLHandlerOptions read GetOptions;
          property InstanceLimit : integer            read GetInstanceLimit;
        public
          function Instantiate : IURLHandler;
      end;

    // TFrame is a wrapper to a IURLHandler. It contains a IURLHandler and also
    // implements it.

    TFrame =
      class( TObject, IURLHandler )
        public
          constructor Create( aMetaFrame : TMetaFrame );
          destructor  Destroy; override;
        private
          fMetaFrame  : TMetaFrame;
          fURLHandler : TURLHandler;
        private
          function GetControl : TControl;
        public
          property Control : TControl read GetControl;
        public
          function HandleURL( URL : string ) : TURLHandlingResult;
          function DisplayStatus( StatusId : TStatusId; var status );
      end;


implementation

end.

