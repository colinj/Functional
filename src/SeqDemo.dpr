program SeqDemo;

uses
  Forms,
  fmMain in 'fmMain.pas' {Form6},
  FunctionalLib.Sequence in 'FunctionalLib.Sequence.pas',
  FunctionalLib.SequenceFunctions in 'FunctionalLib.SequenceFunctions.pas',
  FunctionalLib.Value in 'FunctionalLib.Value.pas',
  uRange in 'uRange.pas',
  uCounter in 'uCounter.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm6, Form6);
  Application.Run;
end.
