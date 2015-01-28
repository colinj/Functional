unit uSequence;

interface

uses
  SysUtils, Classes,
  Generics.Collections,
  DB,
  uValue;

type
  TValueFunc<T, U> = reference to function (Item: TValue<T>): TValue<U>;
  TIteratorProc<T> = reference to procedure (P: TPredicate<T>);
  TFoldFunc<T, U> = reference to function (Item: T; Acc: U): U;


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

  TSequence<T> = record
  private
    FIterate: TIteratorProc<T>;
  public
    function Filter(const aPredicate: TPredicate<T>): TSequence<T, T>;
    function Map<TResult>(const aMapper: TFunc<T, TResult>): TSequence<T, TResult>;
    function Take(const aCount: Integer): TSequence<T, T>;
    function Skip(const aCount: Integer): TSequence<T, T>;
    function TakeWhile(const aPredicate: TPredicate<T>): TSequence<T, T>;
    function SkipWhile(const aPredicate: TPredicate<T>): TSequence<T, T>;
    function Fold<TResult>(const aFoldFunc: TFoldFunc<T, TResult>; const aInitVal: TResult): TResult;
    function ToList: TList<T>;
    procedure ForEach(const aAction: TProc<T>);
  end;

  TSequence = record
  public
    class function From<T>(const aArray: TArray<T>): TSequence<T>; overload; static;
    class function From<T>(const aEnumerable: TEnumerable<T>): TSequence<T>; overload; static;
    class function From(const aString: string): TSequence<Char>; overload; static;
    class function From(const aStrings: TStrings): TSequence<string>; overload; static;
    class function From(const aDataset: TDataSet): TSequence<TDataSet>; overload; static;
  end;

implementation

{ TSequence<T, U> }

constructor TSequence<T, U>.Create(const aIterator: TIteratorProc<T>; const aFunc: TValueFunc<T, U>);
begin
  FIterate := aIterator;
  FFunc := aFunc;
end;

procedure TSequence<T, U>.ForEach(const aAction: TProc<U>);
var
  OldFunc: TValueFunc<T, U>;
  Action: TPredicate<T>;
begin
  OldFunc := FFunc;

  Action :=
    function (Item: T): Boolean
    var
      R: TValue<U>;
    begin
      Result := True;
      R := OldFunc(TValue<T>(Item));
      case R.State of
        vsSomething: aAction(R.Value);
        vsStop: Result := False;
      end;
    end;

  FFunc(TValue<T>.Start);
  FIterate(Action);
end;

function TSequence<T, U>.Fold<TResult>(const aFoldFunc: TFoldFunc<U, TResult>; const aInitVal: TResult): TResult;
var
  OldFunc: TValueFunc<T, U>;
  Accumulator: TResult;
  Folder: TPredicate<T>;
begin
  OldFunc := FFunc;

  Folder :=
    function (Item: T): Boolean
    var
      R: TValue<U>;
    begin
      Result := True;
      R := OldFunc(TValue<T>(Item));
      case R.State of
        vsSomething: Accumulator := aFoldFunc(R.Value, Accumulator);
        vsStop: Result := False;
      end;
    end;

  Accumulator := aInitVal;

  FFunc(TValue<T>.Start);
  FIterate(Folder);

  Result := Accumulator;
end;

function TSequence<T, U>.Filter(const aPredicate: TPredicate<U>): TSequence<T, U>;
var
  OldFunc: TValueFunc<T, U>;
begin
  OldFunc := FFunc;

  Result := TSequence<T, U>.Create(FIterate,
    function (Item: TValue<T>): TValue<U>
    begin
      Result := OldFunc(Item);
      if Result.IsSomething and not aPredicate(Result.Value) then
        Result := TValue<U>.Nothing;
    end);
end;

function TSequence<T, U>.Map<TResult>(const aMapper: TFunc<U, TResult>): TSequence<T, TResult>;
var
  OldFunc: TValueFunc<T, U>;
begin
  OldFunc := FFunc;

  Result := TSequence<T, TResult>.Create(FIterate,
    function (Item: TValue<T>): TValue<TResult>
    var
      R: TValue<U>;
    begin
      R := OldFunc(Item);
      if R.IsSomething then
        Result := TValue<TResult>(aMapper(R.Value))
      else
        Result.SetState(R.State);
    end);
end;

function TSequence<T, U>.Take(const aCount: Integer): TSequence<T, U>;
var
  OldFunc: TValueFunc<T, U>;
  Counter: Integer;
begin
  OldFunc := FFunc;

  Result := TSequence<T, U>.Create(FIterate,
    function (Item: TValue<T>): TValue<U>
    begin
      if Counter = aCount then
        begin
          Result := TValue<U>.Stop;
          Exit;
        end;

      Result := OldFunc(Item);
      case Result.State of
        vsStart: Counter := 0;
        vsSomething: Inc(Counter);
      end;
    end);
end;

function TSequence<T, U>.TakeWhile(const aPredicate: TPredicate<U>): TSequence<T, U>;
var
  OldFunc: TValueFunc<T, U>;
begin
  OldFunc := FFunc;

  Result := TSequence<T, U>.Create(FIterate,
    function (Item: TValue<T>): TValue<U>
    begin
      Result := OldFunc(Item);
      if Result.IsSomething and not aPredicate(Result.Value) then
        Result := TValue<U>.Stop;
    end);
end;

function TSequence<T, U>.ToList: TList<U>;
var
  OldFunc: TValueFunc<T, U>;
  ItemList: TList<U>;
  AddItem: TPredicate<T>;
begin
  OldFunc := FFunc;

  AddItem :=
    function (Item: T): Boolean
    var
      R: TValue<U>;
    begin
      R := OldFunc(TValue<T>(Item));
      if R.IsSomething then
        ItemList.Add(R.Value);
      Result := R.State <> vsStop;
    end;

  ItemList := TList<U>.Create;
  FIterate(AddItem);
  Result := ItemList;
end;

function TSequence<T, U>.Skip(const aCount: Integer): TSequence<T, U>;
var
  OldFunc: TValueFunc<T, U>;
  Counter: Integer;
begin
  OldFunc := FFunc;

  Result := TSequence<T, U>.Create(FIterate,
    function (Item: TValue<T>): TValue<U>
    begin
      Result := OldFunc(Item);
      case Result.State of
        vsStart: Counter := 0;
        vsSomething:
          begin
            Inc(Counter);
            if Counter <= aCount then
              Result := TValue<U>.Nothing;
          end;
      end;
    end);
end;

function TSequence<T, U>.SkipWhile(const aPredicate: TPredicate<U>): TSequence<T, U>;
var
  OldFunc: TValueFunc<T, U>;
  Skipping: Boolean;
begin
  OldFunc := FFunc;

  Result := TSequence<T, U>.Create(FIterate,
    function (Item: TValue<T>): TValue<U>
    begin
      Result := OldFunc(Item);

      case Result.State of
        vsStart: Skipping := True;
        vsSomething:
          begin
            if Skipping then
            begin
              if aPredicate(Result.Value) then
                Result := TValue<U>.Nothing
              else
                Skipping := False;
            end;
          end;
      end;
    end);
end;

{ TSequence<T> }

procedure TSequence<T>.ForEach(const aAction: TProc<T>);
var
  Action: TPredicate<T>;
begin
  Action :=
    function (Item: T): Boolean
    begin
      aAction(Item);
      Result := True;
    end;

  FIterate(Action);
end;

function TSequence<T>.Filter(const aPredicate: TPredicate<T>): TSequence<T, T>;
begin
  Result := TSequence<T, T>.Create(FIterate,
    function (Item: TValue<T>): TValue<T>
    begin
      if Item.IsSomething and not aPredicate(Item.Value) then
        Result := TValue<T>.Nothing
      else
        Result := Item;
    end);
end;

function TSequence<T>.Fold<TResult>(const aFoldFunc: TFoldFunc<T, TResult>; const aInitVal: TResult): TResult;
var
  Accumulator: TResult;
  Folder: TPredicate<T>;
begin
  Accumulator := aInitVal;

  Folder :=
    function (Item: T): Boolean
    begin
      Accumulator := aFoldFunc(Item, Accumulator);
      Result := True;
    end;

  FIterate(Folder);
  Result := Accumulator;
end;

function TSequence<T>.Map<TResult>(const aMapper: TFunc<T, TResult>): TSequence<T, TResult>;
begin
  Result := TSequence<T, TResult>.Create(FIterate,
    function (Item: TValue<T>): TValue<TResult>
    begin
      if Item.IsSomething then
        Result := TValue<TResult>(aMapper(Item.Value))
      else
        Result.SetState(Item.State);
    end);
end;

function TSequence<T>.Skip(const aCount: Integer): TSequence<T, T>;
var
  Counter: Integer;
begin
  Result := TSequence<T, T>.Create(FIterate,
    function (Item: TValue<T>): TValue<T>
    begin
      Result := Item;
      case Result.State of
        vsStart: Counter := 0;
        vsSomething:
          begin
            Inc(Counter);
            if Counter <= aCount then
              Result := TValue<T>.Nothing;
          end;
      end;
    end);
end;

function TSequence<T>.SkipWhile(const aPredicate: TPredicate<T>): TSequence<T, T>;
var
  Skipping: Boolean;
begin
  Result := TSequence<T, T>.Create(FIterate,
    function (Item: TValue<T>): TValue<T>
    begin
      Result := Item;

      case Result.State of
        vsStart: Skipping := True;
        vsSomething:
          begin
            if Skipping then
            begin
              if aPredicate(Result.Value) then
                Result := TValue<T>.Nothing
              else
                Skipping := False;
            end;
          end;
      end;
    end);
end;

function TSequence<T>.Take(const aCount: Integer): TSequence<T, T>;
var
  Counter: Integer;
begin
  Result := TSequence<T, T>.Create(FIterate,
    function (Item: TValue<T>): TValue<T>
    begin
      if Counter = aCount then
        begin
          Result := TValue<T>.Stop;
          Exit;
        end;

      Result := Item;
      case Result.State of
        vsStart: Counter := 0;
        vsSomething: Inc(Counter);
      end;
    end);
end;

function TSequence<T>.TakeWhile(const aPredicate: TPredicate<T>): TSequence<T, T>;
begin
  Result := TSequence<T, T>.Create(FIterate,
    function (Item: TValue<T>): TValue<T>
    begin
      if Item.IsSomething and not aPredicate(Item.Value) then
        Result := TValue<T>.Stop
      else
        Result := Item;
    end);
end;

function TSequence<T>.ToList: TList<T>;
var
  ItemList: TList<T>;
  AddItem: TPredicate<T>;
begin
  AddItem :=
    function (Item: T): Boolean
    begin
      ItemList.Add(Item);
      Result := True;
    end;

  ItemList := TList<T>.Create;
  FIterate(AddItem);
  Result := ItemList;
end;

{ TSequence }

class function TSequence.From<T>(const aArray: TArray<T>): TSequence<T>;
begin
  Result.FIterate :=
    procedure (P: TPredicate<T>)
    var
      Item: T;
    begin
      for Item in aArray do
        if not P(Item) then
          Break;
    end;
end;

class function TSequence.From<T>(const aEnumerable: TEnumerable<T>): TSequence<T>;
begin
  Result.FIterate :=
    procedure (P: TPredicate<T>)
    var
      Item: T;
    begin
      for Item in aEnumerable do
        if not P(Item) then
          Break;
    end;
end;

class function TSequence.From(const aString: string): TSequence<Char>;
begin
  Result.FIterate :=
    procedure (P: TPredicate<Char>)
    var
      Item: Char;
    begin
      for Item in aString do
        if not P(Item) then
          Break;
    end;
end;

class function TSequence.From(const aStrings: TStrings): TSequence<string>;
begin
  Result.FIterate :=
    procedure (P: TPredicate<string>)
    var
      Item: string;
    begin
      for Item in aStrings do
        if not P(Item) then
          Break;
    end;
end;

class function TSequence.From(const aDataset: TDataSet): TSequence<TDataSet>;
begin
  Result.FIterate :=
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
    end;
end;

end.

