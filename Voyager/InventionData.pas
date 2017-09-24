unit InventionData;

interface

  const
    tidMSXML = 'msxml';

  type
    TInventionData =
      class
        public
          constructor Create(url : string);
        private
          fRoot : OleVariant;
        private
          function GetAttr(name : string) : string;
          function GetDesc : string;
        public
          property Attr[name : string] : string read GetAttr;
          property Desc : string read GetDesc;
      end;

implementation

 uses
   ComObj;

 // TInventionData

 constructor TInventionData.Create(url : string);
   var
     O : OleVariant;
   begin
     inherited Create;
     O := CreateOLEObject(tidMSXML);
     try
       O.url := url;
       fRoot := O.root;
     except
       fRoot := Unassigned;
     end;
   end;

 function TInventionData.GetAttr(name : string) : string;
   begin
     try
       if not VarIsEmpty(fRoot)
         then result := fRoot.getAttribute(name)
         else result := '';
     except
       result := '';
     end;
   end;

 function TInventionData.GetDesc : string;
   begin
     try
       if not VarIsEmpty(fRoot)
         then result := fRoot.text
         else result := '';
     except
       result := '';
     end;
   end;

end.
