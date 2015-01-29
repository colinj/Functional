unit uSequence;

interface

uses
  SysUtils, Classes,
  Generics.Collections,
  DB,
  uValue,
  uSequenceFunctions;

type
  TSequence<T, U> = record
  private
    FFunc: TValueFunc<T, U>;
    FIterate: TIteratorProc<T>;
  public
    constructor Create(const aIterator: TIteratorProc<T>; const aFunc: TValueFunc<T, U>);
    function Filter(const aPredicate: TPredicate<U>): TSequence<T, U>;
    function Map<TResult>(const aMapper: TFunc<U, TResult>): TSequence<T, TResult>;
    function Take(const aCount: Integer): TSequence<T, U>;
    function Skip(const aCount: Integer): TSequence<T, U>;
    function TakeWhile(const aPredicate: TPredicate<U>): TSequence<T, U>;
    function SkipWhile(const aPredicate: TPredicate<U>): TSequence<T, U>;
    function Fold<TResult>(const aFoldFunc: TFoldFunc<U, TResult>; const aInitVal: TResult): TResult;
    function ToList: TList<U>;
    procedure ForEach(const aAction: TProc<U>);
  end;

  TSequence = record
  public
    class function From<T>(const aArray: TArray<T>): TSequence<T, T>; overload; static;
    class function From<T>(const aEnumerable: TEnumerable<T>): TSequence<T, T>; overload; static;
    class function From(const aString: string): TSequence<Char, Char>; overload; static;
    class function From(const aStrings: TStrings): TSequence<string, string>; overload; static;
    class function From(const aDataset: TDataSet): TSequence<TDataSet, TDataSet>; overload; static;
    class function Range(const aStart, aFinish: Integer): TSequence<Integer, Integer>; static;
  end;

implementation

{ TSequence<T, U> }

constructor TSequence<T, U>.Create(const aIterator: TIteratorProc<T>; const aFunc: TValueFunc<T, U>);
begin
  FIterate := aIterator;
  FFunc := aFunc;
end;

procedure TSequence<T, U>.ForEach(const aAction: TProc<U>);
begin
  FFunc(TValue<T>.Start);
  FIterate(TSeqFunction<T, U>.CreateAction(FFunc, aAction));
end;

function TSequence<T, U>.Fold<TResult>(const aFoldFunc: TFoldFunc<U, TResult>; const aInitVal: TResult): TResult;
var
  FinalValue: TResult;
begin
  FFunc(TValue<T>.Start);
  FIterate(TSeqFunction<T, U>.CreateFold<TResult>(FFunc, aFoldFunc, aInitVal,
    procedure (aFinalValue: TResult) begin FinalValue := aFinalValue end));
//  FFunc(TValue<T>.Stop);
  Result := FinalValue;
end;

function TSequence<T, U>.Filter(const aPredicate: TPredicate<U>): TSequence<T, U>;
begin
  Result := TSequence<T, U>.Create(FIterate, TSeqFunction<T, U>.CreateFilter(FFunc, aPredicate));
end;

function TSequence<T, U>.Map<TResult>(const aMapper: TFunc<U, TResult>): TSequence<T, TResult>;
begin
  Result := TSequence<T, TResult>.Create(FIterate, TSeqFunction<T, U>.CreateMap<TResult>(FFunc, aMapper));
end;

function TSequence<T, U>.Take(const aCount: Integer): TSequence<T, U>;
begin
  Result := TSequence<T, U>.Create(FIterate, TSeqFunction<T, U>.CreateTake(FFunc, aCount));
end;

function TSequence<T, U>.TakeWhile(const aPredicate: TPredicate<U>): TSequence<T, U>;
begin
  Result := TSequence<T, U>.Create(FIterate, TSeqFunction<T, U>.CreateTakeWhile(FFunc, aPredicate));
end;

function TSequence<T, U>.ToList: TList<U>;
begin
  Result := TList<U>.Create;
  FIterate(TSeqFunction<T, U>.CreateAddItem(FFunc, Result));
end;

function TSequence<T, U>.Skip(const aCount: Integer): TSequence<T, U>;
begin
  Result := TSequence<T, U>.Create(FIterate, TSeqFunction<T, U>.CreateSkip(FFunc, aCount));
end;

function TSequence<T, U>.SkipWhile(const aPredicate: TPredicate<U>): TSequence<T, U>;
begin
  Result := TSequence<T, U>.Create(FIterate, TSeqFunction<T, U>.CreateSkipWhile(FFunc, aPredicate));
end;

{ TSequence }

class function TSequence.From<T>(const aArray: TArray<T>): TSequence<T, T>;
begin
  Result := TSequence<T, T>.Create(
    procedure (P: TPredicate<T>)
    var
      Item: T;
    begin
      for Item in aArray do
        if not P(Item) then
          Break;
    end
    ,
    function (Item: TValue<T>): TValue<T>
    begin
      Result := Item;
    end);
end;

class function TSequence.From<T>(const aEnumerable: TEnumerable<T>): TSequence<T, T>;
begin
  Result := TSequence<T, T>.Create(
    procedure (P: TPredicate<T>)
    var
      Item: T;
    begin
      for Item in aEnumerable do
        if not P(Item) then
          Break;
    end
    ,
    function (Item: TValue<T>): TValue<T>
    begin
      Result := Item;
    end);
end;

class function TSequence.From(const aString: string): TSequence<Char, Char>;
begin
  Result := TSequence<Char, Char>.Create(
    procedure (P: TPredicate<Char>)
    var
      Item: Char;
    begin
      for Item in aString do
        if not P(Item) then
          Break;
    end
    ,
    function (Item: TValue<Char>): TValue<Char>
    begin
      Result := Item;
    end);
end;

class function TSequence.From(const aStrings: TStrings): TSequence<string, string>;
begin
  Result := TSequence<string, string>.Create(
    procedure (P: TPredicate<string>)
    var
      Item: string;
    begin
      for Item in aStrings do
        if not P(Item) then
          Break;
    end
    ,
    function (Item: TValue<string>): TValue<string>
    begin
      Result := Item;
    end);
end;

class function TSequence.From(const aDataset: TDataSet): TSequence<TDataSet, TDataSet>;
begin
  Result := TSequence<TDataSet, TDataSet>.Create(
    procedure (P: TPredicate<TDataSet>)
    var
      D: TDataSet;
    begin
      D := aDataset;
      D.First;
      while not D.Eof do
      begin
        if not P(D) then
          Break;
        D.Next;
      end;
    end
    ,
    function (Item: TValue<TDataSet>): TValue<TDataSet>
    begin
      Result := Item;
    end);
end;

class function TSequence.Range(const aStart, aFinish: Integer): TSequence<Integer, Integer>;
begin
  Result := TSequence<Integer, Integer>.Create(
    procedure (P: TPredicate<Integer>)
    var
      Item: Integer;
    begin
      for Item := aStart to aFinish do
        if not P(Item) then
          Break;
    end
    ,
    function (Item: TValue<Integer>): TValue<Integer>
    begin
      Result := Item;
    end);
end;

end.

