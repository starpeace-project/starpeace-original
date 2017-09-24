unit RDOProtocol;

interface

  // Constants and functions used in the textual protocol

  const
    SelObjCmd   = 'sel';
    IdOfCmd     = 'idof';
    GetPropCmd  = 'get';
    SetPropCmd  = 'set';
    CallMethCmd = 'call';

  const
    ErrorKeyWord  = 'error';
    ResultVarName = 'res';
    ByRefParName  = 'bref';
    ObjIdVarName  = 'objid';

  const
    NameValueSep = '=';
    LiteralDelim = '"';
    Quote        = '''';
    ParamDelim   = ',';
    QueryTerm    = ';';
    Blank        = ' ';

  const
    OrdinalId   = '#';
    SingleId    = '!';
    DoubleId    = '@';
    StringId    = '$';
    OLEStringId = '%';
    VariantId   = '^';
    VoidId      = '*';

  const
    NormPrio      = 'N';
    AboveNormPrio = 'A';
    BelowNormPrio = 'B';
    HighestPrio   = 'H';
    IdlePrio      = 'I';
    LowestPrio    = 'L';
    TimeCritPrio  = 'C';

  const
    CallID   = 'C';
    AnswerID = 'A';

  const
    WhiteSpace = [ #9, #10, #13, #32 ];

implementation

end.
