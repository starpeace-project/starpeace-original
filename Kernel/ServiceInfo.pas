unit ServiceInfo;

interface

  uses
    Kernel, ServiceBlock;

  type
    TServiceInfo =
      class
        private
          fKind     : TMetaService;
          fDemand   : TFluidValue;
          fOffer    : TFluidValue;
          fCapacity : TFluidValue;
          fRatio    : single;
          fQuality  : TPercent;
          fPrice    : word;
        public
          property Kind     : TMetaService read fKind     write fKind;
          property Demand   : TFluidValue  read fDemand   write fDemand;
          property Offer    : TFluidValue  read fOffer    write fOffer;
          property Capacity : TFluidValue  read fCapacity write fCapacity;
          property Ratio    : single       read fRatio    write fRatio;
          property Quality  : TPercent     read fQuality  write fQuality;
          property Price    : word         read fPrice    write fPrice;
      end;

implementation

end.
 