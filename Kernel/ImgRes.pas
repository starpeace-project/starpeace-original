unit ImgRes;

interface

  uses
    Windows, Classes, Collection;

  type
    TScreenRes = (res640x480, res800x600, res1024x768);

  type
    // Classes defined

    TImgDesc = class;
    TImgRes  = class;

    // MetaClasses defined

    CImgDesc = class of TImgDesc;
    CImgRes  = class of TImgRes;

    // TImgDesc describes one of the multiple images that can be used
    // to visually identify some object. Location is the URL (relative)
    // to the image resource. Size is the size in pixels of the image.
    // Resolution is the screen resolution the image was designed for.
    // And TrueColor specifies whether the image was designed for the
    // game|browser palette, or contains true color information.

    TImgDesc =
      class
        public
          constructor Create( aLocation   : string;
                              aSize       : TPoint;
                              aResolution : TScreenRes;
                              aTrueColor  : boolean );
        private
          fLocation   : string;
          fSize       : TPoint;
          fResolution : TScreenRes;
          fTrueColor  : boolean;
        public
          property Location   : string     read fLocation;
          property Resolution : TScreenRes read fResolution;
          property Size       : TPoint     read fSize;
          property TrueColor  : boolean    read fTrueColor;
      end;

    // TImgRes is a set of ImgDesc. A TImgRes should be considered as
    // a single image. Image Handlers will choose the proper TImgDesc
    // for this particular image (TImgRes) according to the visualization
    // context.

    TImgRes =
      class
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fDescriptors : TCollection;
        public
          property Descriptors : TCollection read fDescriptors;
      end;


implementation


  // TImgDesc

  constructor TImgDesc.Create( aLocation   : string;
                               aSize       : TPoint;
                               aResolution : TScreenRes;
                               aTrueColor  : boolean );
    begin
      inherited Create;
      fLocation   := aLocation;
      fSize       := aSize;
      fResolution := aResolution;
      fTrueColor  := aTrueColor;
    end;


  // TImgRes

  constructor TImgRes.Create;
    begin
      inherited;
      fDescriptors := TCollection.Create( 5, 2, rkBelonguer ); 
    end;

  destructor TImgRes.Destroy;
    begin
      fDescriptors.Free;
      inherited;
    end;
    

end.

 