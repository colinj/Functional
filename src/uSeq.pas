unit uSeq;

interface

uses
  SysUtils, Classes,
  Generics.Collections,
  uValue;

type
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
    procedure ForEach(const aAction: TProc<U>);
  end;

implementation

{ TSeq<T, U> }

constructor TSeq<T, U>.Create(const aIterator: TIteratorProc<T>; const aFunc: TValueFunc<T, U>);
begin
  FIterate := aIterator;
  FFunc := aFunc;
end;

procedure TSeq<T, U>.ForEach(const aAction: TProc<U>);
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
      if Result.IsSomething and not aPredicate(Result.Value) then
        Result := TValue<U>.Nothing;
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
      if R.IsSomething then
        Result := TValue<TResult>(aMapper(R.Value))
      else
        Result.SetState(R.State);
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
      if Result.IsSomething and not aPredicate(Result.Value) then
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

end.

