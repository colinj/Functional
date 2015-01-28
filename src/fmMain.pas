unit fmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, uRange, Vcl.Grids, Vcl.DBGrids, Data.DB, Datasnap.DBClient,
  Vcl.ComCtrls, Vcl.ExtCtrls;

type
  TForm6 = class(TForm)
    ClientDataSet1: TClientDataSet;
    DataSource1: TDataSource;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    TabSheet2: TTabSheet;
    Button6: TButton;
    Button7: TButton;
    Panel1: TPanel;
    Panel2: TPanel;
    PageControl2: TPageControl;
    TabSheet3: TTabSheet;
    Memo1: TMemo;
    TabSheet4: TTabSheet;
    DBGrid1: TDBGrid;
    Button8: TButton;
    Button9: TButton;
    TabSheet5: TTabSheet;
    Button10: TButton;
    btnDsLoop: TButton;
    Button11: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure btnDsLoopClick(Sender: TObject);
  private
    procedure PrintNum(X: Integer);
    function DoAction(X: Integer): Boolean;
    procedure Log(const S: string); overload;
    procedure Log(const aFormatStr: string; const Args: array of const); overload;
  end;

   TEmpDetail = record
    Name: string;
    Salary: Double;
    YearsOfService: Integer;
  end;

var
  Form6: TForm6;

implementation

{$R *.dfm}

uses
  DateUtils,
  Generics.Collections,
  uSequence, uCounter;

{ TForm6 }

procedure TForm6.Button1Click(Sender: TObject);
var
  R: TIntegerRange;
  I: TSequence<Integer, Integer>;
  J: Integer;
  S: string;
  I2: TSequence<Integer,Integer>;
begin
  R := TIntegerRange.Create(1, 200);
  try
    I := TSequence.From<Integer>(R);

    J := I.Fold<Integer>(
      function(X, Acc: Integer): Integer begin Result := Acc + X end, 0);

    PrintNum(J);

    S := I.Skip(10).Take(6)
          .Fold<string>(function(X: Integer; Acc: string): string
                        begin
                          if Acc <> '' then
                            Acc := Acc + ',';
                          Result := Acc + IntToStr(X)
                        end, '');
    Memo1.Lines.Add(S);

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

   I2 := TSequence.From<Integer>(R)
      .Filter(function(X: Integer): Boolean begin Result := X mod 3 = 0 end)
      .Map<Integer>(function(X: Integer): Integer begin Result := X * 5 end)
      .Filter(function(X: Integer): Boolean begin Result := X mod 2 = 0 end)
      .Skip(2)
      .Take(2)
      .Map<Integer>(function(X: Integer): Integer begin Result := X + 7 end);

    I2.ForEach(PrintNum);

//    J := TSequence<Integer>(R)
//      .Map<Integer>(function(X: Integer): Integer begin Result := X end)
//      .Take(5)
//      .Fold(function(Acc, X: Integer): Integer begin Result := Acc + X end, 0);

//    TSequence<Integer>(R)
//      .Map<string>(function(X: Integer): string begin Result := IntToStr(X) + ' numbers!' end)
//      .Filter(function(S: string): Boolean begin Result := Copy(S, 1, 1) = '1' end)
//      .Map<string>(function(X: string): string begin Result := Copy(X, 1, 5) end)
//      .ForEach(procedure (S: string) begin Memo1.Lines.Add(S) end);


  finally
    R.Free;
  end;
end;

procedure TForm6.Button2Click(Sender: TObject);
var
  C: Char;
  S: string;
  I: TSequence<Char, Char>;
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
//    I := TSequence<Char>.From(CL);
//    I.ForEach(procedure (C: Char) begin Memo1.Lines.Add(C) end);
//  finally
//    CL.Free;
//  end;

  SetLength(CA2, Length(S));
  for J := 1 to Length(S) do
    CA2[J - 1] := S[J];

  I := TSequence.From<Char>(CA2);
  I.ForEach(procedure (C: Char) begin Memo1.Lines.Add(C) end);

  I := TSequence.From(S);
  I.ForEach(procedure (C: Char) begin Memo1.Lines.Add(C) end);

  TSequence.From(S).ForEach(procedure (C: Char) begin Memo1.Lines.Add(C) end);
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

    TSequence.From(S)
//      .Filter(function (X: string): Boolean begin Result := Copy(X, 1, 1) = 'c' end)
      .ForEach(procedure (X: string) begin Memo1.Lines.Add(X) end);
  finally
    S.Free;
  end;
end;

function BySalary(D: TDataSet): Boolean;
begin
  Result := D.FieldByName('Salary').AsCurrency < 25000
end;

function ToEmpRecord(D: TDataSet):TEmpDetail;
begin
  Result.Name := D.FieldByName('FirstName').AsString + ' ' +
    D.FieldByName('LastName').AsString;
  Result.Salary := D.FieldByName('Salary').AsFloat;
  Result.YearsOfService := YearsBetween(Now, D.FieldByName('HireDate').AsDateTime);
end;

procedure TForm6.btnDsLoopClick(Sender: TObject);
var
  EmpList: TList<TEmpDetail>;
  Detail: TEmpDetail;

begin
  EmpList := TSequence.From(ClientDataSet1)
    .Filter(BySalary)
    .Map<TEmpDetail>(ToEmpRecord)
    .ToList;

//  EmpList := TList<TEmpDetail>.Create;
//  try
//    ClientDataSet1.First;
//    while not ClientDataSet1.Eof do
//    begin
//      if ClientDataSet1.FieldByName('Salary').AsCurrency < 25000 then
//      begin
//        Detail.Name := ClientDataSet1.FieldByName('FirstName').AsString + ' ' + ClientDataSet1.FieldByName('LastName').AsString;
//        Detail.Salary := ClientDataSet1.FieldByName('Salary').AsFloat;
//        Detail.YearsOfService := YearsBetween(Now, ClientDataSet1.FieldByName('HireDate').AsDateTime);
//        EmpList.Add(Detail);
//      end;
//      ClientDataSet1.Next;
//    end;
//
  try
    for Detail in EmpList do
      Log('%s - %m (%d years of service)', [Detail.Name, Detail.Salary, Detail.YearsOfService]);

  finally
    EmpList.Free;
  end;

end;

procedure TForm6.Button5Click(Sender: TObject);
type
  TEmpSummary = record
    Count: Integer;
    Sum: Double;
  end;
var
  EmpDS: TSequence<TDataSet, TDataSet>;
  Total: TEmpSummary;
  S: TList<string>;
  Item: string;
begin
  Total.Count := 0;
  Total.Sum := 0;

  EmpDS := TSequence.From(ClientDataSet1)
    .Filter(function (D: TDataSet): Boolean begin Result := D.FieldByName('Salary').AsCurrency < 20000 end);

  EmpDS
    .ForEach( procedure(D: TDataSet)
              var
                YearsOfService: Integer;
              begin
                YearsOfService := YearsBetween(Now, D.FieldByName('HireDate').AsDateTime);
                Log('%s %s - %m (%d years of service)',
                  [D.FieldByName('FirstName').AsString, D.FieldByName('LastName').AsString,
                  D.FieldByName('Salary').AsFloat, YearsOfService])
              end);
  Memo1.Lines.Add('-----------------');

  Total := EmpDS.Fold<TEmpSummary>(
    function(D: TDataSet; Acc: TEmpSummary): TEmpSummary
                        begin
                          Result.Count := Acc.Count + 1;
                          Result.Sum := Acc.Sum + D.FieldByName('Salary').AsFloat;
                        end, Total);
  Memo1.Lines.Add(Format('Count=%d, Sum=%m, Avg=%n', [Total.Count, Total.Sum, Total.Sum/Total.Count]));
  Memo1.Lines.Add('-----------------');

  S := EmpDS
    .Map<string>(function (D: TDataSet): string
                  begin
                    Result := D.FieldByName('LastName').AsString + ', ' +
                      D.FieldByName('FirstName').AsString;
                  end)
    .ToList;
  try
    S.Sort;
    for Item in S do
      Memo1.Lines.Add(Item);
  finally
    S.Free;
  end;

end;

function TForm6.DoAction(X: Integer): Boolean;
begin
  Memo1.Lines.Add(IntToStr(X));
  Result := x < 11;
end;

procedure TForm6.Log(const aFormatStr: string; const Args: array of const);
begin
  Log(Format(aFormatStr, Args));
end;

procedure TForm6.PrintNum(X: Integer);
begin
  Memo1.Lines.Add(IntToStr(X));
end;

procedure TForm6.Log(const S: string);
begin
  Memo1.Lines.Add(S);
end;


procedure TForm6.Button6Click(Sender: TObject);
var
  A: TFunc<Integer>;
  B: TFunc<Integer>;
  C: TFunc<Integer>;
begin
  A := StaticCounter;
  B := StaticCounter;
  C := A;

  Log('A = %d', [A]);
  Log('A = %d', [A]);
  Log('A = %d', [A]);
  Log('');
  Log('B = %d', [B]);
  Log('B = %d', [B]);
  Log('B = %d', [B]);
  Log('');
  Log('A = %d', [A]);
  Log('A = %d', [A]);
  Log('');
  Log('B = %d', [B]);
  Log('');
  Log('C = %d', [C]);
  Log('C = %d', [C]);
  Log('C = %d', [C]);
end;

procedure TForm6.Button7Click(Sender: TObject);
var
  A: TFunc<Integer>;
  B: TFunc<Integer>;
  C: TFunc<Integer>;
begin
  A := CreateCounter();
  B := CreateCounter();
  C := A;

  Log('A = %d', [A]);
  Log('A = %d', [A]);
  Log('A = %d', [A]);
  Log('');
  Log('B = %d', [B]);
  Log('B = %d', [B]);
  Log('B = %d', [B]);
  Log('');
  Log('A = %d', [A]);
  Log('A = %d', [A]);
  Log('');
  Log('B = %d', [B]);
  Log('');
  Log('C = %d', [C]);
  Log('C = %d', [C]);
  Log('C = %d', [C]);
end;

procedure TForm6.Button8Click(Sender: TObject);
begin
  Memo1.Clear;
end;

procedure TForm6.Button9Click(Sender: TObject);
var
  R: TIntegerRange;
  I: Integer;
begin
  R := TIntegerRange.Create(90000000, 90001000);
  try
    for I in R do
    begin
      if IsPrime(I) then
        Log('%d is a prime number.', [I]);
    end;
    Log('done');
  finally
    R.Free;
  end;
end;

end.
