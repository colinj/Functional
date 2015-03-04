unit fmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Vcl.Grids, Vcl.DBGrids, Data.DB, Datasnap.DBClient,
  Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.ActnList;

type
  TFrmDemo = class(TForm)
    ClientDataSet1: TClientDataSet;
    DataSource1: TDataSource;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Panel1: TPanel;
    PageControl2: TPageControl;
    TabSheet3: TTabSheet;
    Memo1: TMemo;
    TabSheet4: TTabSheet;
    DBGrid1: TDBGrid;
    Button9: TButton;
    TabSheet5: TTabSheet;
    Button10: TButton;
    btnDsLoop: TButton;
    atlDemo: TActionList;
    actDemo01: TAction;
    actDemo02: TAction;
    actDemo03A: TAction;
    actDemo03B: TAction;
    actDemo04: TAction;
    actDemo05: TAction;
    actDemo06: TAction;
    Button6: TButton;
    actDemo07: TAction;
    Button7: TButton;
    actDemo08: TAction;
    Button8: TButton;
    Button11: TButton;
    actDemo09: TAction;
    procedure Button5Click(Sender: TObject);
    procedure btnDsLoopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actDemo01Execute(Sender: TObject);
    procedure actDemo02Execute(Sender: TObject);
    procedure actDemo03AExecute(Sender: TObject);
    procedure actDemo03BExecute(Sender: TObject);
    procedure actDemo04Execute(Sender: TObject);
    procedure actDemo05Execute(Sender: TObject);
    procedure actDemo06Execute(Sender: TObject);
    procedure actDemo07Execute(Sender: TObject);
    procedure actDemo08Execute(Sender: TObject);
    procedure actDemo09Execute(Sender: TObject);
  private
    procedure PrintNum(X: Integer);
    procedure PrintStr(S: string); overload;
    procedure PrintStr(const aFormatStr: string; const Args: array of const); overload;
    procedure PrintTitle(S: string);
    procedure PrintHeader(S: string);
    procedure PrintDone;
  end;

   TEmpDetail = record
    Name: string;
    Salary: Double;
    YearsOfService: Integer;
  end;

var
  FrmDemo: TFrmDemo;

implementation

{$R *.dfm}

uses
  Math,
  DateUtils,
  Generics.Collections,
  Functional.Sequence,
  Functional.FuncFactory;

var
  IntArray_1to10: TArray<Integer>;

{ TFrmDemo }

procedure TFrmDemo.FormCreate(Sender: TObject);
begin
  ClientDataSet1.FileName := 'data\employee.cds';
  ClientDataSet1.Open;

  IntArray_1to10 := TArray<Integer>.Create(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
end;

procedure TFrmDemo.actDemo01Execute(Sender: TObject);
begin
  PrintTitle('Demo 1. Array of Integer = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]');
  PrintHeader('ForEach PrintNum');

  TSeq.From<Integer>(IntArray_1to10)
    .ForEach(PrintNum);

  PrintDone;
end;

function IsEven(I: Integer): Boolean;
begin
  Result := I mod 2 = 0;
end;

procedure TFrmDemo.actDemo02Execute(Sender: TObject);
begin
  PrintTitle('Demo 2. Array of Integer = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]');
  PrintHeader('Where (IsEven) example');

  TSeq.From<Integer>(IntArray_1to10)
    .Where(IsEven)
    .ForEach(PrintNum);

  PrintDone;
end;

procedure TFrmDemo.actDemo03AExecute(Sender: TObject);
var
  Seq: TSeq<Integer, Integer>;
begin
  PrintTitle('Demo 3A. Array of Integer = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]');

  Seq := TSeq.From<Integer>(IntArray_1to10);

  PrintHeader('Select (add 12) returning an integer');
  Seq
    .Select<Integer>(
      function(I: Integer): Integer
      begin
        Result := I + 12;
      end)
    .ForEach(PrintNum);

  PrintHeader('Select (add 12) returning a string)');
  Seq
    .Select<string>(
      function(I: Integer): string
      begin
        Result := Format('Calculation: %d + 12 = %d', [I, I + 12]);
      end)
    .ForEach(PrintStr);

  PrintDone;
end;

function Add12(I: Integer): Integer;
begin
  Result := I + 12;
end;

function Add12String(I: Integer): string;
begin
  Result := Format('Calculation: %d + 12 = %d', [I, Add12(I)]);
end;

procedure TFrmDemo.actDemo03BExecute(Sender: TObject);
var
  Seq: TSeq<Integer, Integer>;
begin
  PrintTitle('Demo 3B. Array of Integer = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]');

  Seq := TSeq.From<Integer>(IntArray_1to10);

  PrintHeader('Select (add 12) - returning an integer');
  Seq
    .Select<Integer>(Add12)
    .ForEach(PrintNum);

  PrintHeader('Select (add 12) returning a string');
  Seq
    .Select<string>(Add12String)
    .ForEach(PrintStr);

  PrintDone;
end;

procedure TFrmDemo.actDemo04Execute(Sender: TObject);
var
  Seq: TSeq<Integer, Integer>;
begin
  PrintTitle('Demo 4. Array of Integer = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]');

  Seq := TSeq.From<Integer>(IntArray_1to10);

  PrintHeader('Take the first 5 items');
  Seq
    .Take(5)
    .ForEach(PrintNum);

  PrintHeader('Skip the first 6 items');
  Seq
    .Skip(6)
    .ForEach(PrintNum);

  PrintHeader('Skip the first 3 items and Take the next 6 items');
  Seq
    .Skip(3)
    .Take(6)
    .ForEach(PrintNum);

  PrintDone;
end;

procedure TFrmDemo.actDemo05Execute(Sender: TObject);
var
  Seq: TSeq<Integer, Integer>;
begin
  PrintTitle('Demo 5. Array of Integer = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]');

  Seq := TSeq.From<Integer>(IntArray_1to10);

  PrintHeader('TakeWhile value of item < 7');
  Seq
    .TakeWhile(function(Item: Integer): Boolean begin Result := Item < 7 end)
    .ForEach(PrintNum);

  PrintHeader('Skip first 2 items then TakeWhile item value < 9');
  Seq
    .Skip(2)
    .TakeWhile(function(Item: Integer): Boolean begin Result := Item < 9 end)
    .ForEach(PrintNum);

  PrintDone;
end;

procedure TFrmDemo.actDemo06Execute(Sender: TObject);
var
  Seq: TSeq<Integer, Integer>;
  LessThan: TFunc<Integer, TPredicate<Integer>>;
begin
  PrintTitle('Demo 6. Array of Integer = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]');

  Seq := TSeq.From<Integer>(IntArray_1to10);

  PrintHeader('SkipWhile value of item <= 3');
  Seq
    .SkipWhile(function (Item: Integer): Boolean begin Result := Item <= 3 end)
    .ForEach(PrintNum);

  PrintHeader('SkipWhile item value < 5 then Take the next 3 items');
  Seq
    .SkipWhile(function (Item: Integer): Boolean begin Result := Item < 5 end)
    .Take(3)
    .ForEach(PrintNum);

  // LessThan is an anonymous function that returns another anonymous function!
  LessThan :=
    function (Num: Integer): TPredicate<Integer>
    begin
      Result :=
        function(I: Integer): Boolean
        begin
          Result := I < Num;
        end;
    end;

  PrintHeader('SkipWhile item value < 2 then TakeWhile item value < 9');
  Seq
    .SkipWhile(LessThan(2))
    .TakeWhile(LessThan(9))
    .ForEach(PrintNum);

  PrintDone;
end;

procedure TFrmDemo.actDemo07Execute(Sender: TObject);
var
  Numbers: TArray<Integer>;
  SumInts: TFoldFunc<Integer, Integer>;
  Total: Integer;
  CommaSeparate: TFoldFunc<Integer, string>;
  CsvRec: string;
begin
  PrintTitle('Demo 7. Array of Integer = [1, 5, 5, 9, 23, 4, 10]');

  Numbers := TArray<Integer>.Create(1, 5, 5, 9, 23, 4, 10);

  PrintHeader('Items in the array');
  TSeq.From<Integer>(Numbers)
    .ForEach(PrintNum);

  PrintHeader('Fold example - Sum items in an array.');
  SumInts :=
    function(I: Integer; Accumulator: Integer): Integer
    begin
        Result := Accumulator + I;
    end;

  Total := TSeq
    .From<Integer>(Numbers)
    .Fold<Integer>(SumInts, 0);

  PrintStr('Total = %d', [Total]);

  PrintHeader('Fold example - Append items into a comma-separated string.');
  CommaSeparate :=
    function(I: Integer; Accumulator: string): string
    begin
      if Accumulator <> '' then
        Accumulator := Accumulator + ',';
      Result := Accumulator + IntToStr(I)
    end;

  CsvRec := TSeq
    .From<Integer>(Numbers)
    .Fold<string>(CommaSeparate, '');

  PrintStr(CsvRec);

  PrintDone;
end;

procedure TFrmDemo.actDemo08Execute(Sender: TObject);
const
  ALPHA_CHARS = ['a'..'z', 'A'..'Z'];
  VOWELS = ['a','e','i','o','u','A','E','I','O','U'];
var
  S: string;
begin
  S := 'Hello, World!';

  PrintTitle(Format('Demo 8. string = ''%s''', [S]));

  PrintHeader('ForEach over a string value');
  TSeq.From(S)
    .ForEach(procedure (C: Char) begin PrintStr('The character = ''%s''', [C]) end);

  PrintHeader('Filter for vowels only');
  TSeq.From(S)
    .Filter(function (C: Char): Boolean begin Result := CharInSet(C, VOWELS); end)
    .ForEach(procedure (C: Char) begin PrintStr('The character = ''%s''', [C]) end);

  PrintHeader('Filter for consonants and captalise');
  TSeq.From(S)
    .Filter(function (C: Char): Boolean begin Result := CharInSet(C, ALPHA_CHARS) and not CharInSet(C, VOWELS); end)
    .Map<Char>(function (C: Char): Char begin Result := UpCase(C) end)
    .ForEach(procedure (C: Char) begin PrintStr('The character = ''%s''', [C]) end);

  PrintDone;
end;

procedure TFrmDemo.actDemo09Execute(Sender: TObject);
var
  Animals: TStringList;
begin
  Animals := TStringList.Create;
  try
    PrintTitle('Demo 9. A list of animals in a TStringList');
    Animals.CommaText :=
      'cat,dog,mouse,horse,cockatoo,pig,bear,goat,cow,' +
      'sheep,cobra,rabbit,crab,lion,tiger,cougar,snake';

    PrintHeader('ForEach over a TStrings object');
    TSeq.From(Animals)
      .ForEach(PrintStr);

    PrintHeader('Filter for strings starting with ''c''');
    TSeq.From(Animals)
      .Filter(function (S: string): Boolean begin Result := Copy(S, 1, 1) = 'c' end)
      .ForEach(PrintStr);
  finally
    Animals.Free;
  end;

  PrintDone;
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

procedure TFrmDemo.btnDsLoopClick(Sender: TObject);
var
  EmpList: TList<TEmpDetail>;
  Detail: TEmpDetail;

begin
  EmpList := TSeq.From(ClientDataSet1)
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
      PrintStr('%s - %m (%d years of service)', [Detail.Name, Detail.Salary, Detail.YearsOfService]);

  finally
    EmpList.Free;
  end;

end;

procedure TFrmDemo.Button5Click(Sender: TObject);
type
  TEmpSummary = record
    Count: Integer;
    Sum: Double;
  end;
const
  ZERO_VAL: TEmpSummary = (Count: 0; Sum: 0);
var
  EmpDS: TSeq<TDataSet, TDataSet>;
  Total: TEmpSummary;
  S: TList<string>;
  Item: string;
begin
  EmpDS := TSeq.From(ClientDataSet1)
    .Filter(function (D: TDataSet): Boolean begin Result := D.FieldByName('Salary').AsCurrency < 20000 end);

  EmpDS
    .ForEach( procedure(D: TDataSet)
              var
                YearsOfService: Integer;
              begin
                YearsOfService := YearsBetween(Now, D.FieldByName('HireDate').AsDateTime);
                PrintStr('%s %s - %m (%d years of service)',
                  [D.FieldByName('FirstName').AsString, D.FieldByName('LastName').AsString,
                  D.FieldByName('Salary').AsFloat, YearsOfService])
              end);
  Memo1.Lines.Add('-----------------');

  Total := EmpDS.Fold<TEmpSummary>(
    function(D: TDataSet; Acc: TEmpSummary): TEmpSummary
    begin
      Result.Count := Acc.Count + 1;
      Result.Sum := Acc.Sum + D.FieldByName('Salary').AsFloat;
    end, ZERO_VAL);
  Memo1.Lines.Add(Format('Count=%d, Sum=%m, Avg=%f', [Total.Count, Total.Sum, Total.Sum/Total.Count]));
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

procedure TFrmDemo.PrintStr(const aFormatStr: string; const Args: array of const);
begin
  PrintStr(Format(aFormatStr, Args));
end;

procedure TFrmDemo.PrintStr(S: string);
begin
  Memo1.Lines.Add(S);
end;

procedure TFrmDemo.PrintNum(X: Integer);
begin
  PrintStr(IntToStr(X));
end;

procedure TFrmDemo.PrintTitle(S: string);
begin
  Memo1.Clear;
  PrintStr(S);
  PrintStr(StringOfChar('=', Max(39, Length(S)) + 1));
end;

procedure TFrmDemo.PrintHeader(S: string);
begin
  PrintStr('');
  PrintStr(StringOfChar('-', Length(S) + 1));
  PrintStr(S);
  PrintStr(StringOfChar('-', Length(S) + 1));
end;

procedure TFrmDemo.PrintDone;
begin
  PrintStr('');
  PrintStr(StringOfChar('=', 40));
  PrintStr('Done!');
end;

end.