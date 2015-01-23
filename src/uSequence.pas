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

  TValueFunc<T, U> = reference to function(X: TValue<T>): TValue<U>;

  TSeq<T, U> = record
  private
    FFunc: TValueFunc<T, U>;
    FEnumerable: TEnumerable<T>;
    procedure MakeFilter(const aOldFunc: TValueFunc<T, U>; const aPredicate: TPredicate<U>);
    procedure MakeMap<OldU>(const aOldFunc: TValueFunc<T, OldU>; const aMapper: TFunc<OldU, U>);
    procedure MakeTake(const aOldFunc: TValueFunc<T, U>; const aCount: Integer);
    procedure MakeSkip(const aOldFunc: TValueFunc<T, U>; const aCount: Integer);
  public
    constructor Create(const aEnumerator: TEnumerable<T>; const aFunc: TValueFunc<T, U>);
    function Filter(const aPredicate: TPredicate<U>): TSeq<T, U>;
    function Map<TResult>(const aMapper: TFunc<U, TResult>): TSeq<T, TResult>;
    function Take(const aCount: Integer): TSeq<T, U>;
    function Skip(const aCount: Integer): TSeq<T, U>;
    procedure DoIt(const aAction: TProc<U>);
  end;

  TSeq<T> = record
  private
    FEnumerable: TEnumerable<T>;
  public
    class operator Implicit(const aEnumerator: TEnumerable<T>): TSeq<T>;
    function Filter(const aPredicate: TPredicate<T>): TSeq<T, T>;
    function Map<TResult>(const aMapper: TFunc<T, TResult>): TSeq<T, TResult>;
    procedure DoIt(const aAction: TProc<T>);
  end;

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

constructor TSeq<T, U>.Create(const aEnumerator: TEnumerable<T>; const aFunc: TValueFunc<T, U>);
begin
  FEnumerable := aEnumerator;
  FFunc := aFunc;
end;

procedure TSeq<T, U>.DoIt(const aAction: TProc<U>);
var
  Item: T;
  R: TValue<U>;
begin
  FFunc(TValue<T>.Start);
  for Item in FEnumerable do
  begin
    R := FFunc(TValue<T>(Item));
    case R.State of
      vsSomething: aAction(R.Value);
      vsStop: Break;
    end;
  end;
end;

procedure TSeq<T, U>.MakeFilter(const aOldFunc: TValueFunc<T, U>; const aPredicate: TPredicate<U>);
begin
  FFunc :=
    function (X: TValue<T>): TValue<U>
    begin
      Result := aOldFunc(X);
      if (Result.State = vsSomething) and not aPredicate(Result.Value) then
        Result.FState := vsNothing;
    end;
end;

procedure TSeq<T, U>.MakeMap<OldU>(const aOldFunc: TValueFunc<T, OldU>; const aMapper: TFunc<OldU, U>);
begin
  FFunc :=
    function (X: TValue<T>): TValue<U>
    var
      R: TValue<OldU>;
    begin
      R := aOldFunc(X);
      if R.State = vsSomething then
        Result := TValue<U>(aMapper(R.Value))
      else
        Result := TValue<U>.Nothing;
    end;
end;

procedure TSeq<T, U>.MakeTake(const aOldFunc: TValueFunc<T, U>; const aCount: Integer);
var
  Counter: Integer;
begin
  FFunc :=
    function (X: TValue<T>): TValue<U>
    begin
      if Counter = aCount then
        begin
          Result := TValue<U>.Stop;
          Exit;
        end;

      Result := aOldFunc(X);
      case Result.State of
        vsStart: Counter := 0;
        vsSomething: Inc(Counter);
      end;
    end;
end;

procedure TSeq<T, U>.MakeSkip(const aOldFunc: TValueFunc<T, U>; const aCount: Integer);
var
  Counter: Integer;
begin
  FFunc :=
    function (X: TValue<T>): TValue<U>
    begin
      Result := aOldFunc(X);
      case Result.State of
        vsStart: Counter := 0;
        vsSomething:
          begin
            Inc(Counter);
            if Counter <= aCount then
              Result := TValue<U>.Nothing;
          end;
      end;
    end;
end;

function TSeq<T, U>.Filter(const aPredicate: TPredicate<U>): TSeq<T, U>;
begin
  Result.FEnumerable := FEnumerable;
  Result.MakeFilter(FFunc, aPredicate);
end;

function TSeq<T, U>.Map<TResult>(const aMapper: TFunc<U, TResult>): TSeq<T, TResult>;
begin
  Result.FEnumerable := FEnumerable;
  Result.MakeMap<U>(FFunc, aMapper);
end;

function TSeq<T, U>.Take(const aCount: Integer): TSeq<T, U>;
begin
  Result.FEnumerable := FEnumerable;
  Result.MakeTake(FFunc, aCount);
end;

function TSeq<T, U>.Skip(const aCount: Integer): TSeq<T, U>;
begin
  Result.FEnumerable := FEnumerable;
  Result.MakeSkip(FFunc, aCount);
end;

{ TSeq<T> }

class operator TSeq<T>.Implicit(const aEnumerator: TEnumerable<T>): TSeq<T>;
begin
  Result.FEnumerable := aEnumerator;
end;

procedure TSeq<T>.DoIt(const aAction: TProc<T>);
var
  Item: T;
begin
  for Item in FEnumerable do
  begin
    aAction(Item);
  end;
end;

function TSeq<T>.Filter(const aPredicate: TPredicate<T>): TSeq<T, T>;
begin
  Result := TSeq<T, T>.Create(FEnumerable,
    function (X: TValue<T>): TValue<T>
    begin
      Result := X;
      if (Result.State = vsSomething) and not aPredicate(Result.Value) then
        Result.FState := vsNothing;
    end);
end;

function TSeq<T>.Map<TResult>(const aMapper: TFunc<T, TResult>): TSeq<T, TResult>;
begin
  Result := TSeq<T, TResult>.Create(FEnumerable,
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

end.

