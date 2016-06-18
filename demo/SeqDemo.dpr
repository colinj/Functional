program SeqDemo;

uses
  Forms,
  fmMain in 'fmMain.pas' {FrmDemo},
  Functional.FuncFactory in '..\src\Functional.FuncFactory.pas',
  Functional.Sequence in '..\src\Functional.Sequence.pas',
  Functional.Value in '..\src\Functional.Value.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmDemo, FrmDemo);
  Application.Run;
end.
