unit VerObjectImages;

interface

  type
    TVersionableObjectImage =
      class
        public
          constructor Create(Indent, loBound, hiBound : integer);
          destructor  Destroy; override;
        private
          fProps : TStringList;
        public
          procedure Load(
      end;

implementation

end.

// CLOSE THE DOOR...
 