unit CircuitEquivalences;

interface

  uses
    Collection, Circuits;

  type
    TEquivalence  = class;
    TEquivalences = class;

    TEquivalence =
      class
        public
          constructor Create(aRep : TCircuit);
          destructor  Destroy; override;
        private
          fCircuits : TCollection;
        public
          property Circuits : TCollection read fCircuits;
        public
          function Belongs(aCircuit : TCircuit) : boolean;
          function Include(aCircuit : TCircuit) : boolean;
          function Merge(anEqv : TEquivalence) : boolean;
      end;

    TEquivalences =
      class
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fEquivalences : TCollection;
        public
          function  FindEquivalence(Circuit : TCircuit) : TEquivalence;
          procedure AddEquivalence(Circuit1, Circuit2 : TCircuit);
          procedure Clear;
        public
          property Equivalences : TCollection read fEquivalences;
      end;

implementation

  // TEquivalence

  constructor TEquivalence.Create(aRep : TCircuit);
    begin
      inherited Create;
      fCircuits := TCollection.Create(0, rkUse);
      fCircuits.Insert(aRep);
    end;

  destructor TEquivalence.Destroy;
    begin
      fCircuits.Free;
      inherited;
    end;

  function TEquivalence.Belongs(aCircuit : TCircuit) : boolean;
    begin
      result := fCircuits.IndexOf(aCircuit) <> noIndex;
    end;

  function TEquivalence.Include(aCircuit : TCircuit) : boolean;
    begin
      if not Belongs(aCircuit)
        then
          begin
            fCircuits.Insert(aCircuit);
            result := true;
          end
        else result := false;
    end;

  function TEquivalence.Merge(anEqv : TEquivalence) : boolean;
    var
      i : integer;
    begin
      if anEqv <> nil
        then
          begin
            for i := 0 to pred(anEqv.fCircuits.Count) do
              Include(TCircuit(anEqv.fCircuits[i]));
            result := true;
          end
        else result := false;
    end;


  // TEquivalences

  constructor TEquivalences.Create;
    begin
      inherited Create;
      fEquivalences := TCollection.Create(0, rkBelonguer);
    end;

  destructor TEquivalences.Destroy;
    begin
      fEquivalences.Free;
      inherited;
    end;

  function TEquivalences.FindEquivalence(Circuit : TCircuit) : TEquivalence;
    var
      i : integer;
    begin
      i := 0;
      while (i < fEquivalences.Count) and not TEquivalence(fEquivalences[i]).Belongs(Circuit) do
        inc(i);
      if i < fEquivalences.Count
        then result := TEquivalence(fEquivalences[i])
        else result := nil;
    end;

  procedure TEquivalences.AddEquivalence(Circuit1, Circuit2 : TCircuit);
    var
      Eq1, Eq2 : TEquivalence;
    begin
      Eq1 := FindEquivalence(Circuit1);
      Eq2 := FindEquivalence(Circuit2);
      if (Eq1 = nil) or (Eq1 <> Eq2)
        then
          if Eq1 <> nil
            then
              if Eq2 <> nil
                then
                  begin
                    Eq1.Merge(Eq2);
                    fEquivalences.Delete(Eq2);
                  end
                else Eq1.Include(Circuit2)
            else
              if Eq2 <> nil
                then
                  begin
                    Eq2.Include(Circuit1);
                    Eq2.Include(Circuit2);
                  end
                else
                  begin
                    Eq1 := TEquivalence.Create(Circuit1);
                    Eq1.Include(Circuit2);
                    fEquivalences.Insert(Eq1);
                  end;
    end;

  procedure TEquivalences.Clear;
    begin
      fEquivalences.DeleteAll;
    end;

end.
