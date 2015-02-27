program SeqDemo;

uses
  Forms,
  fmMain in 'fmMain.pas' {Form6},
  FuncLib.Sequence in 'FuncLib.Sequence.pas',
  FuncLib.SequenceFunctions in 'FuncLib.SequenceFunctions.pas',
  FuncLib.Value in 'FuncLib.Value.pas',
  uRange in 'uRange.pas',
  uCounter in 'uCounter.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm6, Form6);
  Application.Run;
end.
