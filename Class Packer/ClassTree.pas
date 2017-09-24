unit ClassTree;

interface

  uses
    Collection;

  type
    TClassTreeNode  = class;
    TClassTreeLeave = class;

    TClassTreeNode =
      class
        public
          constructor Create(aDepth : byte; aBaseNum : word);
          destructor  Destroy; override;
        private
          fDepth    : byte;
          fBaseNum  : word;
          fChildren : TCollection;
        private
          function Compare(Item1, Item2 : TObject) : integer;
      end;

    TClassTreeLeave =
      class
      end;

implementation

  // TClassTreeNode

  constructor TClassTreeNode.Create(aDepth : byte; aBaseNum : word);
    begin
      inherited Create;
      fDepth    := aDepth;
      fBaseNum  := aBaseNum;
      fChildren := TSortedCollection.Create(0, rkBelonguer, );
    end;

  destructor TClassTreeNode.Destroy;
    begin
    end;

  function TClassTreeNode.Compare(Item1, Item2 : TObject) : integer;
    begin
      if Item1.ClassType = TClassTreeNode
        then result := TClassTreeNode(Item1).fBaseNum - TClassTreeNode(Item2).fBaseNum
        else result := TClassTreeLeave(); // >>
    end;

end.
