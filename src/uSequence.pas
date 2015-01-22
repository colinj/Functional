unit uSequence;

interface

uses
  SysUtils, Classes,
  Generics.Collections;

type
//  TValueState = (vsStart, vsSomething, vsNothing, vsStop);

  TOptional<T> = record
    FValue: T;
    FIsSomething: Boolean;
  public
    constructor Create(const aValue: T; const aValidFlag: Boolean = True);
    property Value: T read FValue;
    property IsSomething: Boolean read FIsSomething;
  end;

  TValueFunc<T, U> = reference to function(X: TOptional<T>): TOptional<U>;

  TSeq<T, U> = record
  private
    FFunc: TValueFunc<T, U>;
    FEnumerable: TEnumerable<T>;
    procedure MakeFilter(const aOldFunc: TValueFunc<T, U>; const aPredicate: TPredicate<U>);
    procedure MakeMap<OldU>(const aOldFunc: TValueFunc<T, OldU>; const aMapper: TFunc<OldU, U>);
  public
    constructor Create(const aEnumerator: TEnumerable<T>; const aFunc: TValueFunc<T, U>);
    function Filter(const aPredicate: TPredicate<U>): TSeq<T, U>;
    function Map<TResult>(const aMapper: TFunc<U, TResult>): TSeq<T, TResult>;
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

{ TOptional<T> }

constructor TOptional<T>.Create(const aValue: T; const aValidFlag: Boolean);
begin
  FValue := aValue;
  FIsSomething := aValidFlag;
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
  R: TOptional<U>;
begin
  for Item in FEnumerable do
  begin
    R := FFunc(TOptional<T>.Create(Item));
    if R.IsSomething then
      aAction(R.Value);
  end;
end;

function TSeq<T, U>.Filter(const aPredicate: TPredicate<U>): TSeq<T, U>;
begin
  Result.FEnumerable := FEnumerable;
  Result.MakeFilter(FFunc, aPredicate);
end;

procedure TSeq<T, U>.MakeFilter(const aOldFunc: TValueFunc<T, U>; const aPredicate: TPredicate<U>);
begin
  FFunc :=
    function (X: TOptional<T>): TOptional<U>
    var
      R: TOptional<U>;
    begin
      R := aOldFunc(X);
      if R.IsSomething then
        Result := TOptional<U>.Create(R.Value, aPredicate(R.Value))
      else
        Result.FIsSomething := False;
    end;
end;

function TSeq<T, U>.Map<TResult>(const aMapper: TFunc<U, TResult>): TSeq<T, TResult>;
begin
  Result.FEnumerable := FEnumerable;
  Result.MakeMap<U>(FFunc, aMapper);
end;

procedure TSeq<T, U>.MakeMap<OldU>(const aOldFunc: TValueFunc<T, OldU>; const aMapper: TFunc<OldU, U>);
begin
  FFunc :=
    function (X: TOptional<T>): TOptional<U>
    var
      R: TOptional<OldU>;
    begin
      R := aOldFunc(X);
      if R.IsSomething then
        Result := TOptional<U>.Create(aMapper(R.Value))
      else
        Result.FIsSomething := False;
    end;
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
    function (X: TOptional<T>): TOptional<T>
    begin
      Result := TOptional<T>.Create(X.Value, aPredicate(X.Value))
    end);
end;

function TSeq<T>.Map<TResult>(const aMapper: TFunc<T, TResult>): TSeq<T, TResult>;
begin
  Result := TSeq<T, TResult>.Create(FEnumerable,
    function (X: TOptional<T>): TOptional<TResult>
    begin
      Result := TOptional<TResult>.Create(aMapper(X.Value))
    end);
end;

end.

