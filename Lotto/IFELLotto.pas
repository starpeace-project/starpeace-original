unit ifelLotto;

interface

  uses
    Collection;

  const
    TICKET_LEN    = 3;
    TICKET_MAX    = 20;
    TICKET_PRICE  = 100000000;
    IFEL_PERC     = 50;
    TWO_WIN_PERC  = 10;

  type
    TLottoTicket = class;
    TLottoSystem = class;

    TLottoTicket =
      class
        public
          constructor Create(anOwner : TObject); overload;
          constructor Create(anOwner : TObject; str : string); overload;
        protected
          procedure GenerateTicket;
          procedure Sort;
        private
          fOwner   : TObject;
          fNumbers : array[0..TICKET_LEN-1] of byte;
        private
          function  GetNumber(index : integer) : byte;
        public
          function HasNumber(num : byte) : boolean;
          function NumbersMatched(other : TLottoTicket) : integer;
          function AsText : string;
        public
          property Owner : TObject read fOwner write fOwner;
          property Numbers[index : integer] : byte read GetNumber;
      end;

    TLottoFeedback = procedure(Winner : TObject; money : currency) of object;

    TLottoSystem =
      class
        public
          constructor Create;
          destructor  Destroy; override;
        private
          fTickets : TLockableCollection;
          fPrevPot : currency;
          fWinner  : TLottoTicket;
        public
          procedure AddTicket(Ticket : TLottoTicket);
          procedure RemTicketsFrom(Owner : TObject);
          procedure Compute(Notify : TLottoFeedback);
        private
          function GetTicketCount : integer;
          function GetTicket(index : integer) : TLottoTicket;
          function GetJackpot : currency;
        public
          property Jackpot : currency read GetJackpot;
          property TicketCount : integer read GetTicketCount;
          property Tickets[index : integer] : TLottoTicket read GetTicket; default;
        public
          function GetTicketsOf(Owner : TObject) : string;
      end;


implementation

  uses
    SysUtils, CompStringsParser;

  // TLottoTicket

  constructor TLottoTicket.Create(anOwner : TObject);
    begin
      inherited Create;
      fOwner := anOwner;
      GenerateTicket;
    end;

  constructor TLottoTicket.Create(anOwner : TObject; str : string);
    var
      aux  : string;
      p, i : integer;
    begin
      inherited Create;
      fOwner := anOwner;
      i := 0;
      p := 1;
      aux := CompStringsParser.GetNextStringUpTo(str, p, ',');
      while (aux <> '') and (i < TICKET_LEN) do
        begin
          fNumbers[i] := StrToInt(aux);
          inc(p);
          inc(i);
          aux := CompStringsParser.GetNextStringUpTo(str, p, ',');
        end;
      Sort;
    end;

  procedure TLottoTicket.GenerateTicket;
    var
      i : integer;
      n : byte;
    begin
      for i := 0 to pred(TICKET_LEN) do
        begin
          n := random(TICKET_MAX) + 1;
          while HasNumber(n) do
            n := random(TICKET_MAX) + 1;
          fNumbers[i] := n;
        end;
      Sort;
    end;

  procedure TLottoTicket.Sort;
    var
      i, j : integer;
      tmp  : byte;
    begin
      for i := 0 to pred(TICKET_LEN) do
        for j := succ(i) to pred(TICKET_LEN) do
          if fNumbers[i] > fNumbers[j]
            then
              begin
                tmp := fNumbers[i];
                fNumbers[i] := fNumbers[j];
                fNumbers[j] := tmp;
              end;
    end;

  function TLottoTicket.GetNumber(index : integer) : byte;
    begin
      if index < TICKET_LEN
        then result := fNumbers[index]
        else result := 0;
    end;

  function TLottoTicket.HasNumber(num : byte) : boolean;
    var
      j : integer;
    begin
      j := 0;
      while (j < TICKET_LEN) and (num <> fNumbers[j]) do
        inc(j);
      result := j < TICKET_LEN;
    end;

  function TLottoTicket.NumbersMatched(other : TLottoTicket) : integer;
    var
      i : integer;
    begin
      result := 0;
      for i := 0 to pred(TICKET_LEN) do
        if other.HasNumber(fNumbers[i])
          then inc(result);
    end;

  function TLottoTicket.AsText : string;
    var
      i : integer;
    begin
      result := '';
      for i := 0 to pred(TICKET_LEN) do
        if i <> 0
          then result := result + ',' + IntToStr(fNumbers[i])
          else result := IntToStr(fNumbers[i]);
    end;


  // TLottoSystem

  constructor TLottoSystem.Create;
    begin
      inherited;
      fTickets := TLockableCollection.Create(0, rkBelonguer);
    end;

  destructor TLottoSystem.Destroy;
    begin
      fTickets.Free;
      inherited;
    end;

  procedure TLottoSystem.AddTicket(Ticket : TLottoTicket);
    begin
      fTickets.Insert(Ticket);
    end;

  function TLottoSystem.GetTicketCount : integer;
    begin
      result := fTickets.Count;
    end;

  function TLottoSystem.GetTicket(index : integer) : TLottoTicket;
    begin
      result := TLottoTicket(fTickets[index]);
    end;

  function TLottoSystem.GetJackpot : currency;
    begin
      result := TICKET_PRICE*fTickets.Count + fPrevPot;
    end;

  function TLottoSystem.GetTicketsOf(Owner : TObject) : string;
    var
      i      : integer;
      Ticket : TLottoTicket;
    begin
      result := '';
      fTickets.Lock;
      try
        for i := pred(fTickets.Count) downto 0 do
          begin
            Ticket := TLottoTicket(fTickets[i]);
            if Ticket.Owner = Owner
              then result := Ticket.AsText + '|' + result;
          end;
      finally
        fTickets.Unlock;
      end;
    end;

  procedure TLottoSystem.RemTicketsFrom(Owner : TObject);
    var
      i : integer;
    begin
      fTickets.Lock;
      try
        for i := pred(fTickets.Count) downto 0 do
          if TLottoTicket(fTickets[i]).Owner = Owner
            then fTickets.AtDelete(i);
      finally
        fTickets.Unlock;
      end;
    end;

  procedure TLottoSystem.Compute(Notify : TLottoFeedback);
    var
      totalPrize : currency;
      jkpCount   : integer;
      win2of3    : integer;
      i          : integer;
      jackpotWin : currency;
      twoWinners : currency;
      payJackpot : currency;
      payTwoWin  : currency;
      Ticket     : TLottoTicket;
    begin
      // Compute Total Prize
      totalPrize := ((100 - IFEL_PERC)/100)*TICKET_PRICE*fTickets.Count + fPrevPot;
      // Pick the winner
      fWinner.Free;
      fWinner := TLottoTicket.Create(nil);
      // Compute winner tickets
      jkpCount := 0;
      win2of3  := 0;
      fTickets.Lock;
      try
        for i := pred(fTickets.Count) downto 0 do
          case fWinner.NumbersMatched(TLottoTicket(fTickets[i])) of
            0, 1 : fTickets.AtDelete(i);
            2    : inc(win2of3);
            3    : inc(jkpCount);
          end;
      finally
        fTickets.Unlock;
      end;
      // Compute Prizes
      twoWinners := (TWO_WIN_PERC/100)*totalPrize;
      jackpotWin := totalPrize - twoWinners;
      if jkpCount > 0
        then payJackpot := jackpotWin/jkpCount
        else payJackpot := 0;
      if win2of3 > 0
        then payTwoWin := twoWinners/win2of3
        else payTwoWin := 0;
      // Compute Next Turn Jackpot
      fPrevPot := totalPrize - twoWinners - jackpotWin;
      if fPrevPot < 100
        then fPrevPot := 0;
      // Pay the tickets
      try
        for i := pred(fTickets.Count) downto 0 do
          begin
            Ticket := TLottoTicket(TLottoTicket(fTickets[i]));
            case fWinner.NumbersMatched(Ticket) of
              2 : Notify(Ticket.Owner, payTwoWin);
              3 : Notify(Ticket.Owner, payJackpot);
            end;
          end;
      finally
        fTickets.Unlock;
      end;
    end;

begin

  randomize;

end.
