unit uSequence;

interface

uses
  SysUtils, Classes,
  Generics.Collections;

type
  TValueState = (vsStart, vsSomething, vsNothing, vsStop);

  TValue<T> = record
    FValue: T;
    FState: TValueState;
  public
    class operator Implicit(const aValue: T): TValue<T>;
    class function Nothing: TValue<T>; static;
    class function Start: TValue<T>; static;
    class function Stop: TValue<T>; static;
    property Value: T read FValue;
    property State: TValueState read FState;
  end;

  TValueFunc<T, U> = reference to function (X: TValue<T>): TValue<U>;
  TFoldFunc<T, U> = reference to function (X: T; Acc: U): U;
  TIteratorProc<T> = reference to procedure (P: TPredicate<T>);

  TSeq<T, U> = record
  private
    FFunc: TValueFunc<T, U>;
    FIterate: TIteratorProc<T>;
  public
    constructor Create(const aIterator: TIteratorProc<T>; const aFunc: TValueFunc<T, U>);
    function Filter(const aPredicate: TPredicate<U>): TSeq<T, U>;
    function Map<TResult>(const aMapper: TFunc<U, TResult>): TSeq<T, TResult>;
    function Take(const aCount: Integer): TSeq<T, U>;
    function Skip(const aCount: Integer): TSeq<T, U>;
    function TakeWhile(const aPredicate: TPredicate<U>): TSeq<T, U>;
    function SkipWhile(const aPredicate: TPredicate<U>): TSeq<T, U>;
    function Fold<TResult>(const aFoldFunc: TFoldFunc<U, TResult>; const aInitVal: TResult): TResult;
    procedure DoIt(const aAction: TProc<U>);
  end;

  TSeq<T> = record
  private
    FIterate: TIteratorProc<T>;
  public
//    class operator Implicit(const aEnumerator: TEnumerable<T>): TSeq<T>;
    class function From(aEnumerable: TEnumerable<T>): TSeq<T>; static;
    function Filter(const aPredicate: TPredicate<T>): TSeq<T, T>;
    function Map<TResult>(const aMapper: TFunc<T, TResult>): TSeq<T, TResult>;
    function Take(const aCount: Integer): TSeq<T, T>;
    function Skip(const aCount: Integer): TSeq<T, T>;
    function TakeWhile(const aPredicate: TPredicate<T>): TSeq<T, T>;
    function SkipWhile(const aPredicate: TPredicate<T>): TSeq<T, T>;
    function Fold<TResult>(const aFoldFunc: TFoldFunc<T, TResult>; const aInitVal: TResult): TResult;
    procedure DoIt(const aAction: TProc<T>);
  end;
{
  TStringEnumerable = class(TEnumerable<Char>)
  private
    FStringVal: string;
  public
    class operator Implicit(const aString: string): TStringEnumerable;
    function
  end;
  }
implementation

{ TValue<T> }

class operator TValue<T>.Implicit(const aValue: T): TValue<T>;
begin
  Result.FValue := aValue;
  Result.FState := vsSomething;
end;

class function TValue<T>.Nothing: TValue<T>;
begin
  Result.FState := vsNothing;
end;

class function TValue<T>.Start: TValue<T>;
begin
  Result.FState := vsStart;
end;

class function TValue<T>.Stop: TValue<T>;
begin
  Result.FState := vsStop;
end;

{ TSeq<T, U> }

constructor TSeq<T, U>.Create(const aIterator: TIteratorProc<T>; const aFunc: TValueFunc<T, U>);
begin
  FIterate := aIterator;
  FFunc := aFunc;
end;

procedure TSeq<T, U>.DoIt(const aAction: TProc<U>);
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

function TSeq<T, U>.Fold<TResult>(const aFoldFunc: TFoldFunc<U, TResult>; const aInitVal: TResult): TResult;
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

function TSeq<T, U>.Filter(const aPredicate: TPredicate<U>): TSeq<T, U>;
var
  OldFunc: TValueFunc<T, U>;
begin
  OldFunc := FFunc;

  Result := TSeq<T, U>.Create(FIterate,
    function (X: TValue<T>): TValue<U>
    begin
      Result := OldFunc(X);
      if (Result.State = vsSomething) and not aPredicate(Result.Value) then
        Result.FState := vsNothing;
    end);
end;

function TSeq<T, U>.Map<TResult>(const aMapper: TFunc<U, TResult>): TSeq<T, TResult>;
var
  OldFunc: TValueFunc<T, U>;
begin
  OldFunc := FFunc;

  Result := TSeq<T, TResult>.Create(FIterate,
    function (X: TValue<T>): TValue<TResult>
    var
      R: TValue<U>;
    begin
      R := OldFunc(X);
      if R.State = vsSomething then
        Result := TValue<TResult>(aMapper(R.Value))
      else
        Result := TValue<TResult>.Nothing;
    end);
end;

function TSeq<T, U>.Take(const aCount: Integer): TSeq<T, U>;
var
  OldFunc: TValueFunc<T, U>;
  Counter: Integer;
begin
  OldFunc := FFunc;

  Result := TSeq<T, U>.Create(FIterate,
    function (X: TValue<T>): TValue<U>
    begin
      if Counter = aCount then
        begin
          Result := TValue<U>.Stop;
          Exit;
        end;

      Result := OldFunc(X);
      case Result.State of
        vsStart: Counter := 0;
        vsSomething: Inc(Counter);
      end;
    end);
end;

function TSeq<T, U>.TakeWhile(const aPredicate: TPredicate<U>): TSeq<T, U>;
var
  OldFunc: TValueFunc<T, U>;
begin
  OldFunc := FFunc;

  Result := TSeq<T, U>.Create(FIterate,
    function (X: TValue<T>): TValue<U>
    begin
      Result := OldFunc(X);
      if (Result.State = vsSomething) and not aPredicate(Result.Value) then
        Result := TValue<U>.Stop;
    end);
end;

function TSeq<T, U>.Skip(const aCount: Integer): TSeq<T, U>;
var
  OldFunc: TValueFunc<T, U>;
  Counter: Integer;
begin
  OldFunc := FFunc;

  Result := TSeq<T, U>.Create(FIterate,
    function (X: TValue<T>): TValue<U>
    begin
      Result := OldFunc(X);
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

function TSeq<T, U>.SkipWhile(const aPredicate: TPredicate<U>): TSeq<T, U>;
var
  OldFunc: TValueFunc<T, U>;
  Skipping: Boolean;
begin
  OldFunc := FFunc;

  Result := TSeq<T, U>.Create(FIterate,
    function (X: TValue<T>): TValue<U>
    begin
      Result := OldFunc(X);

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

{ TSeq<T> }
{
class operator TSeq<T>.Implicit(const aEnumerator: TEnumerable<T>): TSeq<T>;
begin
  Result.FEnumerable := aEnumerator;
end;
}
procedure TSeq<T>.DoIt(const aAction: TProc<T>);
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

function TSeq<T>.Filter(const aPredicate: TPredicate<T>): TSeq<T, T>;
begin
  Result := TSeq<T, T>.Create(FIterate,
    function (X: TValue<T>): TValue<T>
    begin
      Result := X;
      if (Result.State = vsSomething) and not aPredicate(Result.Value) then
        Result.FState := vsNothing;
    end);
end;

function TSeq<T>.Fold<TResult>(const aFoldFunc: TFoldFunc<T, TResult>; const aInitVal: TResult): TResult;
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

class function TSeq<T>.From(aEnumerable: TEnumerable<T>): TSeq<T>;
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

function TSeq<T>.Map<TResult>(const aMapper: TFunc<T, TResult>): TSeq<T, TResult>;
begin
  Result := TSeq<T, TResult>.Create(FIterate,
    function (X: TValue<T>): TValue<TResult>
    begin
      if X.State = vsSomething then
      begin
        Result := TValue<TResult>(aMapper(X.Value));
        Result.FState := vsSomething;
      end
      else
        Result.FState := X.State;
    end);
end;

function TSeq<T>.Skip(const aCount: Integer): TSeq<T, T>;
var
  Counter: Integer;
begin
  Result := TSeq<T, T>.Create(FIterate,
    function (X: TValue<T>): TValue<T>
    begin
      Result := X;
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

function TSeq<T>.SkipWhile(const aPredicate: TPredicate<T>): TSeq<T, T>;
var
  Skipping: Boolean;
begin
  Result := TSeq<T, T>.Create(FIterate,
    function (X: TValue<T>): TValue<T>
    begin
      Result := X;

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

function TSeq<T>.Take(const aCount: Integer): TSeq<T, T>;
var
  Counter: Integer;
begin
  Result := TSeq<T, T>.Create(FIterate,
    function (X: TValue<T>): TValue<T>
    begin
      if Counter = aCount then
        begin
          Result := TValue<T>.Stop;
          Exit;
        end;

      Result := X;
      case Result.State of
        vsStart: Counter := 0;
        vsSomething: Inc(Counter);
      end;
    end);
end;

function TSeq<T>.TakeWhile(const aPredicate: TPredicate<T>): TSeq<T, T>;
begin
  Result := TSeq<T, T>.Create(FIterate,
    function (X: TValue<T>): TValue<T>
    begin
      Result := X;
      if (Result.State = vsSomething) and not aPredicate(Result.Value) then
        Result := TValue<T>.Stop;
    end);
end;

end.

