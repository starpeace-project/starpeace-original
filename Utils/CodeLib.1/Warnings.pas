unit Warnings;

interface


uses
  Classes;


type
  hverb = cardinal;

type
  TWarningTarget = TObject;

type
  IWarningInformant =
    interface
      procedure AttachTarget(which : TWarningTarget);
      procedure DetachTarget(which : TWarningTarget);
    end;


implementation

end.
