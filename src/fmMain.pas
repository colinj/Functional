unit fmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, uRange;

type
  TForm6 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    procedure PrintNum(X: Integer);
    function DoAction(X: Integer): Boolean;
  end;

var
  Form6: TForm6;

implementation

{$R *.dfm}

uses
  Generics.Collections,
  uSequence;

{ TForm6 }

procedure TForm6.Button1Click(Sender: TObject);
var
  R: TIntegerRange;
  I: TSeq<Integer>;
  I2: TSeq<Integer, Integer>;
//  S: TList<string>;
  J: Integer;
  S: string;
begin
  R := TIntegerRange.Create(1, 20);
  try
    I := TSeq<Integer>.From(R);
//    I2 := TSeq<Integer>(R)
//      .Filter(function(X: Integer): Boolean begin Result := X mod 3 = 0 end)
//      .Map<Integer>(function(X: Integer): Integer begin Result := X * 5 end)
//      .Filter(function(X: Integer): Boolean begin Result := X mod 2 = 0 end)
//      .Skip(2)
//      .Take(2);
//      .Map<Integer>(function(X: Integer): Integer begin Result := X + 7 end);

//    I2.ForEach(PrintNum);

//    J := TSeq<Integer>(R)
//      .Map<Integer>(function(X: Integer): Integer begin Result := X end)
//      .Take(5)
//      .Fold(function(Acc, X: Integer): Integer begin Result := Acc + X end, 0);
    J := I.Fold<Integer>(function(X, Acc: Integer): Integer begin Result := Acc + X end, 0);
    PrintNum(J);
    S := I.Skip(10).Take(6)
          .Fold<string>(function(X: Integer; Acc: string): string
                        begin
                          if Acc <> '' then
                            Acc := Acc + ',';
                          Result := Acc + IntToStr(X)
                        end, '');
    Memo1.Lines.Add(S);
//    TSeq<Integer>(R)
//      .Map<string>(function(X: Integer): string begin Result := IntToStr(X) + ' numbers!' end)
//      .Filter(function(S: string): Boolean begin Result := Copy(S, 1, 1) = '1' end)
//      .Map<string>(function(X: string): string begin Result := Copy(X, 1, 5) end)
//      .ForEach(procedure (S: string) begin Memo1.Lines.Add(S) end);

  Memo1.Lines.Add('-----------------');
  I
    .TakeWhile(function(X: Integer): Boolean begin Result := X < 13 end)
    .ForEach(PrintNum);

  Memo1.Lines.Add('-----------------');
  I
    .SkipWhile(function(X: Integer): Boolean begin Result := X < 16 end)
    .ForEach(PrintNum);

  Memo1.Lines.Add('-----------------');
  I
    .SkipWhile(function(X: Integer): Boolean begin Result := X < 7 end)
    .TakeWhile(function(X: Integer): Boolean begin Result := X < 16 end)
    .ForEach(PrintNum);

  finally
    R.Free;
  end;
end;

procedure TForm6.Button2Click(Sender: TObject);
var
  C: Char;
  S: string;
  I: TSeq<Char>;
  CA: array[0..10] of Char;
  CA2: TArray<Char>;
  CL: TList<Char>;
  J: Integer;
begin
  S := 'Hello, World!';
//  SetLength(CA, Length(S));
//  StrLCopy(PChar(@CA[0]), PChar(S), Length(CA));

//  CL := TList<Char>.Create;
//  try
//    CL.AddRange(CA);
//    I := TSeq<Char>.From(CL);
//    I.ForEach(procedure (C: Char) begin Memo1.Lines.Add(C) end);
//  finally
//    CL.Free;
//  end;

  SetLength(CA2, Length(S));
  for J := 1 to Length(S) do
    CA2[J - 1] := S[J];

  I := TSeq<Char>.From(CA2);
  I.ForEach(procedure (C: Char) begin Memo1.Lines.Add(C) end);

  I := TSeqString.From(S);
  I.ForEach(procedure (C: Char) begin Memo1.Lines.Add(C) end);


end;

procedure TForm6.Button3Click(Sender: TObject);
var
  Acc: Integer;
  Adder: TPredicate<Integer>;

  procedure Iterate(P: TPredicate<Integer>);
  var
    I: Integer;
  begin
    for I := 1 to 100 do
      if not P(I) then
        Break;
  end;

begin
  Acc := 0;

  Adder :=
    function (X: Integer): Boolean
    begin
      Acc := Acc + X;
      Result := True;
    end;

  Iterate(DoAction);
  Iterate(Adder);
  Memo1.Lines.Add(IntToStr(Acc));

end;

procedure TForm6.Button4Click(Sender: TObject);
var
  S: TStringList;
begin
  S := TStringList.Create;
  try
    S.CommaText := 'cat,dog,mouse,horse,pig,bear,goat,cow,sheep,rabbit,lion,tiger,cougar,snake';

    TSeqStringList.From(S)
//      .Filter(function (X: string): Boolean begin Result := Copy(X, 1, 1) = 'c' end)
      .ForEach(procedure (X: string) begin Memo1.Lines.Add(X) end);
  finally
    S.Free;
  end;
end;

function TForm6.DoAction(X: Integer): Boolean;
begin
  Memo1.Lines.Add(IntToStr(X));
  Result := x < 11;
end;

procedure TForm6.PrintNum(X: Integer);
begin
  Memo1.Lines.Add(IntToStr(X));
end;

end.
