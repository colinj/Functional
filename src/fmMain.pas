unit fmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, uRange;

type
  TForm6 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    procedure PrintNum(X: Integer);
  end;

var
  Form6: TForm6;

implementation

{$R *.dfm}

uses uSequence;
//, Generics.Collections;

{ TForm6 }

procedure TForm6.Button1Click(Sender: TObject);
var
  R: TIntegerRange;
  I: TSeq<Integer>;
  I2: TSeq<Integer, Integer>;
//  S: TList<string>;
begin
  R := TIntegerRange.Create(1, 50);
  try
    I := TSeq<Integer>(R);
    I2 := TSeq<Integer>(R)
      .Filter(function(X: Integer): Boolean begin Result := X mod 3 = 0 end)
      .Map<Integer>(function(X: Integer): Integer begin Result := X * 5 end)
      .Filter(function(X: Integer): Boolean begin Result := X mod 2 = 0 end)
      .Skip(2)
      .Take(2);
//      .Map<Integer>(function(X: Integer): Integer begin Result := X + 7 end);

    I2.DoIt(PrintNum);

//    TSeq<Integer>(R)
//      .Map<string>(function(X: Integer): string begin Result := IntToStr(X) + ' numbers!' end)
//      .Filter(function(S: string): Boolean begin Result := Copy(S, 1, 1) = '1' end)
//      .Map<string>(function(X: string): string begin Result := Copy(X, 1, 5) end)
//      .DoIt(procedure (S: string) begin Memo1.Lines.Add(S) end);
  finally
    R.Free;
  end;
end;

procedure TForm6.PrintNum(X: Integer);
begin
  Memo1.Lines.Add(IntToStr(X));
end;

end.
