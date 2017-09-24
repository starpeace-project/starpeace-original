program StarVtest;

uses
  Forms,
  MainFrm in 'MainFrm.pas' {MainForm},
  FIFOIntf in '..\Interfaces\FIFOIntf.pas',
  CompressIntf in '..\Interfaces\CompressIntf.pas',
  VoiceRx in 'VoiceRx\VoiceRx.pas',
  VoiceTx in 'VoiceTx\VoiceTx.pas',
  WaveIn in 'WaveInOut\WaveIn.pas',
  DSoundUtils in 'WaveInOut\DSoundUtils.pas',
  ExtTimer in 'WaveInOut\ExtTimer.pas',
  MMCheck in 'WaveInOut\MMCheck.pas',
  WaveHdrs in 'WaveInOut\WaveHdrs.pas',
  DSoundOut in 'WaveInOut\DSoundOut.pas',
  D3Math in '..\Utils\D3Math.pas',
  ACM in '..\Utils\ACM.pas',
  WaveConvert in '..\Utils\WaveConvert.pas',
  FIFOUtils in '..\Utils\FIFOUtils.pas',
  FIFO in '..\Utils\FIFO.pas',
  CodecIntf in '..\Interfaces\CodecIntf.pas',
  DSound in '..\..\DirectX\DSound.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
