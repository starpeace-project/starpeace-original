unit TxtRes;

interface

  type
    TTxtResType = (txtPlain, txtHTML);

    // TTxtRes describes a textual resource. Location is the URL (relative)
    // to the resource. TxtType specifies the type of the text.

    TTxtRes =
      class
        public
          constructor Create( aLocation : string; aTxtType : TTxtResType );
        private
          fLocation : string;
          fTxtType  : TTxtResType;
        public
          property Location : string      read fLocation;
          property TxtType  : TTxtResType read fTxtType;
      end;

implementation

  // TTxtRes

  constructor TTxtRes.Create( aLocation : string; aTxtType : TTxtResType );
    begin
      inherited Create;
      fLocation := aLocation;
      fTextType := aTextType;
    end;
    
end.


