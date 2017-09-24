program CeperoMP3;

uses
  Forms,
  MP3Reader in 'MP3Reader.pas',
  MP3Player in 'MP3Player.pas',
  DirectSound in '..\DelphiX\DirectSound.pas',
  Direct3D in '..\DelphiX\Direct3D.pas',
  DirectDraw in '..\DelphiX\DirectDraw.pas',
  DXCommon in '..\DelphiX\DXCommon.pas',
  DSoundOut in '..\WaveInOut\DSoundOut.pas',
  ExtTimer in '..\WaveInOut\ExtTimer.pas',
  DSoundUtils in '..\WaveInOut\DSoundUtils.pas',
  D3Math in '..\Utils\D3Math.pas',
  DShow in '..\DXMedia\DShow.pas',
  PlayFrm in 'PlayFrm.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
