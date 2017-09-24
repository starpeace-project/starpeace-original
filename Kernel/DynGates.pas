unit DynGates;

interface

  uses
    Kernel;

  type
    TDynamicInput =
      class( TMetaGate )
        public
          constructor Create( aName          : string;
                              aGateClass     : CGate;
                              aMetaFluid     : TMetaFluid;
                              aSize          : integer;
                              anOptions      : TMetaGateOptions;
                              aFluidDataSize : integer;
                              anOffset       : TOffset );
      end;

implementation

end.
