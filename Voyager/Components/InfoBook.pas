unit InfoBook;

interface

  uses
    Classes, ExtCtrls, Controls, Messages{, Buffer, TileManager, TilePanel};

  type
    TInfoBook = class;

    CInfoBookPage = class of TInfoBookPage;
    TInfoBookPage =
      class( TCustomPanel )
        public
          constructor Create( anOwner : TComponent ); override;
          destructor  Destroy;                        override;
        public
          procedure MoveTo( ord : integer );
        protected
          procedure PageSelected;   virtual;
          procedure PageUnselected; virtual;
        private
          fPageName : string;
          fIndex    : integer;
        protected
          procedure SetPageName( aPageName : string ); virtual;
          function  GetInfoBook : TInfoBook;           virtual;
        published
          property InfoBook : TInfoBook read GetInfoBook;
          property PageName : string    read fPageName write SetPageName;
          property Index    : integer   read fIndex write fIndex;
          property Align;
          property Alignment;
          property BevelInner;
          property BevelOuter;
          property BevelWidth;
          property BorderWidth;
          property BorderStyle;
          property DragCursor;
          property DragMode;
          property Enabled;
          property Caption;
          property Color;
          property Ctl3D;
          property Font;
          property Locked;
          property ParentColor;
          property ParentCtl3D;
          property ParentFont;
          property ParentShowHint;
          property PopupMenu;
          property ShowHint;
          property TabOrder;
          property TabStop;
          property Visible;
          property OnClick;
          property OnDblClick;
          property OnDragDrop;
          property OnDragOver;
          property OnEndDrag;
          property OnEnter;
          property OnExit;
          property OnMouseDown;
          property OnMouseMove;
          property OnMouseUp;
          property OnResize;
          property OnStartDrag;
        public
          property Canvas;
          {
        private
          fTileInfo : TTileInfo;
          }
        private
          fExposing : boolean;
        private
          procedure WMEraseBkgnd(var Message : TMessage); message WM_ERASEBKGND;
        public
          property Exposing : boolean read fExposing;
      end;

    TPageOperation  = (popInsertion, popSelection, popPropertyChange, popDeletion);
    TOnPageModified = procedure( Operation : TPageOperation; Page : TInfoBookPage ) of object;

    TInfoBook =
      class( TCustomPanel )
        public
          constructor Create( anOwner : TComponent ); override;
          destructor  Destroy;                        override;
        protected
          class function PageClass : CInfoBookPage; virtual;
        public
          function  AddPage : TInfoBookPage;         virtual;
          procedure DeletePage( PageIdx : integer ); virtual;
          procedure AddNewPage        ( Page : TInfoBookPage );
          procedure RemoveReferencesTo( Page : TInfoBookPage );
          procedure DeleteAllPages;
        private
          fActivePage     : TInfoBookPage;
          fOnPageModified : TOnPageModified;
        protected
          function  GetPageIndex : integer;                     virtual;
          procedure SetPageIndex( PageIdx : integer );          virtual;
          procedure SetActivePage( Page : TInfoBookPage );      virtual;
          function  GetPage( index : integer ) : TInfoBookPage; virtual;
          function  GetPageCount : integer;
        public
          property PageCount : integer read GetPageCount;
          property Pages[index : integer] : TInfoBookPage read GetPage;
        published
          property ActivePage     : TInfoBookPage   read fActivePage     write SetActivePage;
          property PageIndex      : integer         read GetPageIndex    write SetPageIndex;
          property OnPageModified : TOnPageModified read fOnPageModified write fOnPageModified;
        published
          property Align;
          property Alignment;
          property BevelInner;
          property BevelOuter;
          property BevelWidth;
          property BorderWidth;
          property BorderStyle;
          property DragCursor;
          property DragMode;
          property Enabled;
          property Caption;
          property Color;
          property Ctl3D;
          property Font;
          property Locked;
          property ParentColor;
          property ParentCtl3D;
          property ParentFont;
          property ParentShowHint;
          property PopupMenu;
          property ShowHint;
          property TabOrder;
          property TabStop;
          property Visible;
          property OnClick;
          property OnDblClick;
          property OnDragDrop;
          property OnDragOver;
          property OnEndDrag;
          property OnEnter;
          property OnExit;
          property OnMouseDown;
          property OnMouseMove;
          property OnMouseUp;
          property OnResize;
          property OnStartDrag;
        private
          procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
      end;


  procedure Register;


implementation

  uses
    Windows
     {$IFDEF VER140}
       , DesignIntf 
     {$ELSE}
       , Dsgnintf
     {$ENDIF}  //.rag
{, Collection};
    

  // TInfoBookPage

  constructor TInfoBookPage.Create( anOwner : TComponent );
    begin
      inherited;
      BevelInner := bvNone;
      BevelOuter := bvNone;
      Align      := alClient;
    end;

  destructor TInfoBookPage.Destroy;
    begin
      //fInfoBook.RemoveReferencesTo( self );
      inherited;
    end;

  procedure TInfoBookPage.MoveTo( ord : integer );
    {
    var
      Coll  : TCollection;
      delta : integer;
      i     : integer;
    }
    begin
      {
      Coll := TCollection.Create( 10, 5, rkUse );
      for i := 0 to pred(InfoBook.ControlCount) do
        Coll.Insert( InfoBook.Pages[i] );
      delta := abs(Index - ord) div (Index - ord);
      i     := ord;
      while i <> Index do
        begin
          (Coll[i] as TInfoBookPage).Index := i + delta;
          inc( i, delta );
        end;
      Index := ord;
      Coll.Free;
      }
    end;

  procedure TInfoBookPage.PageSelected;
    begin
      if (InfoBook <> nil) and assigned(InfoBook.OnPageModified)
        then InfoBook.OnPageModified( popSelection, self );
    end;

  procedure TInfoBookPage.PageUnselected;
    begin
    end;
    
  procedure TInfoBookPage.SetPageName( aPageName : string );
    begin
      fPageName := aPageName;
      if (InfoBook <> nil) and assigned(InfoBook.OnPageModified)
        then InfoBook.OnPageModified( popPropertyChange, self );
    end;
    
  function TInfoBookPage.GetInfoBook : TInfoBook;
    begin
      if Parent <> nil
        then result := Parent as TInfoBook
        else result := nil;
    end;

  procedure TInfoBookPage.WMEraseBkgnd(var Message: TMessage);
    begin
      Message.Result := 1;
    end;

  // TInfoBook

  constructor TInfoBook.Create( anOwner : TComponent );
    begin
      inherited;
      BevelInner := bvNone;
      BevelOuter := bvNone;
      Align      := alClient;
      Caption    := '';
    end;

  destructor TInfoBook.Destroy;
    begin
      inherited;
    end;

  class function TInfoBook.PageClass : CInfoBookPage;
    begin
      result := TInfoBookPage;
    end;

  function TInfoBook.AddPage : TInfoBookPage;
    begin
      result := PageClass.Create( self );
      AddNewPage( result );
    end;

  procedure TInfoBook.DeletePage( PageIdx : integer );
    var
      Page : TInfoBookPage;
    begin
      Page := Pages[PageIdx];
      if assigned(OnPageModified)
        then OnPageModified( popDeletion, Page );
      RemoveReferencesTo( Page );
      Page.Parent := nil;
      Page.Free;
      if ControlCount > 0
        then PageIndex := 0;
    end;

  procedure TInfoBook.AddNewPage( Page : TInfoBookPage );
    begin
      {
      if Page.TileName = ''
        then Page.TileName := TileName;
      }
      if (Page.Owner <> nil) and (Page.Owner <> self)
        then
          begin
            Page.Owner.RemoveComponent( Page );
            InsertComponent( Page );
          end;
      Page.Caption   := '';
      Page.Visible   := false;
      Page.Parent    := self;
      Page.fIndex    := pred(ControlCount);
      //Page.PopupMenu := PopupMenu;
      ActivePage     := Page;
      if assigned(OnPageModified)
        then OnPageModified( popInsertion, Page );
    end;

  procedure TInfoBook.RemoveReferencesTo( Page : TInfoBookPage );
    var
      i : integer;
    begin
      if not (csDestroying in ComponentState)
        then
          begin
            for i := 0 to pred(ControlCount) do
              if (Controls[i] as TInfoBookPage).Index > Page.Index
                then dec((Controls[i] as TInfoBookPage).fIndex);
            if Page = ActivePage
              then ActivePage := nil;
          end;
    end;

  procedure TInfoBook.DeleteAllPages;
    begin
      while PageCount > 0 do
        DeletePage(0);
    end;

  function TInfoBook.GetPageIndex : integer;
    begin
      if ActivePage <> nil
        then result := ActivePage.Index
        else result := -1;
    end;

  procedure TInfoBook.SetPageIndex( PageIdx : integer );
    begin
      if PageIdx <> -1
        then ActivePage := Pages[PageIdx]
        else ActivePage := nil;
    end;

  procedure TInfoBook.SetActivePage( Page : TInfoBookPage );
    begin
      if Page <> ActivePage
        then
          begin
            if Page <> nil
              then
                begin
                  Page.fExposing := true;
                  Page.Visible := true;
                  Page.SetZOrder( true );
                  Page.fExposing := false;
                end;
            if ActivePage <> nil
              then
                begin
                  ActivePage.PageUnselected;
                  ActivePage.Visible := false;
                end;
            fActivePage := Page;
          end;
      if ActivePage <> nil
        then ActivePage.PageSelected;
    end;

  function TInfoBook.GetPageCount : integer;
    begin
      result := ControlCount;
    end;

  function TInfoBook.GetPage( index : integer ) : TInfoBookPage;
    var
      i : integer;
    begin
      i := 0;
      while (i < ControlCount) and ((Controls[i] as TInfoBookPage).Index <> index) do
        inc( i );
      if i < ControlCount
        then result := TInfoBookPage(Controls[i])
        else result := nil;
    end;


  procedure TInfoBook.WMEraseBkgnd(var Message: TMessage);
    begin
      Message.Result := 1;
    end;

  // Design time stuff:

   {$IFDEF VER140}
          // TBaseComponentEditor e implementar IComponentEditor y IDefaultEditor, 
   {$ELSE}
  type
    TInfoBookEditor =
      class( TDefaultEditor )
        procedure ExecuteVerb( Index : integer );           override;
        function  GetVerb    ( Index : integer ) : string;  override;
        function  GetVerbCount                   : integer; override;
      end;
    {$ENDIF}

   {$IFDEF VER140}
   {$ELSE}
  procedure TInfoBookEditor.ExecuteVerb( Index : integer );
    var
      Book         : TInfoBook;
      InfoBookPage : TInfoBookPage;
      Designer     : IFormDesigner;   //.rag
      i            : integer;
    begin
      if Index = 0
        then
          begin
            if Component is TInfoBookPage
              then Book := TInfoBook(TInfoBookPage(Component).Parent)
              else Book := TInfoBook(Component);
            if Book <> nil
              then
                begin
                  Designer := Self.Designer;
                  InfoBookPage := Book.PageClass.Create(Designer.Form);
                  Book.AddNewPage( InfoBookPage );
                  try
                    InfoBookPage.Name    := Designer.UniqueName(TInfoBookPage.ClassName);
                    InfoBookPage.Caption := '';
                    for i := 0 to pred(InfoBookPage.ComponentCount) do
                      InfoBookPage.Components[i].Name := Designer.UniqueName(InfoBookPage.Components[i].ClassName);
                    InfoBookPage.Parent := Book;
                  except
                    InfoBookPage.Free;
                    raise;
                  end;
                  Designer.SelectComponent(InfoBookPage);
                  Designer.Modified;
                end;
          end;
    end;

  function TInfoBookEditor.GetVerb( Index : integer ) : string;
    begin
      result := '&Add Page';
    end;

  function TInfoBookEditor.GetVerbCount : integer;
    begin
      result := 1;
    end;
 {$ENDIF}

  procedure Register;
    begin
      RegisterComponents( 'Merchise', [TInfoBook] );
   {$IFDEF VER140}
   {$ELSE}
      RegisterComponentEditor(TInfoBook, TInfoBookEditor);
      RegisterComponentEditor(TInfoBookPage, TInfoBookEditor);
   {$EndIf}
    end;



end.


