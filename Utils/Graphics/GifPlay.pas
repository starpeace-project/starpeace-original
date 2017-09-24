unit GifPlay;

interface

  uses
    Windows, SysUtils, Classes,
    ChunkStream, Gifs, Dibs;

  type
    TBufferRec =
      record
        Owned  : boolean;
        Header : PDib;
        Pixels : pointer;
      end;

  type
    EGifInvalid = Exception;
(*
   type
     TGifStream =
       class( TChunkStream )
          procedure LoadFrameHeader;                                                           override;
          function  FrameHeaderSize : integer;                                                 override;
          function  CurrentFrameSize : integer;                                                override;
       end;
*)
implementation

end.
