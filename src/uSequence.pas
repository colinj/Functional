unit uSequence;

interface

uses
  SysUtils, Classes,
  Generics.Collections,
  uValue, uSeq;

type
  TSequence<T> = record
  private
    FIterate: TIteratorProc<T>;
  public
    class operator Implicit(const aArray: TArray<T>): TSequence<T>; overload;
    class operator Implicit(const aEnumerable: TEnumerable<T>): TSequence<T>; overload;
    function Filter(const aPredicate: TPredicate<T>): TSeq<T, T>;
    function Map<TResult>(const aMapper: TFunc<T, TResult>): TSeq<T, TResult>;
    function Take(const aCount: Integer): TSeq<T, T>;
    function Skip(const aCount: Integer): TSeq<T, T>;
    function TakeWhile(const aPredicate: TPredicate<T>): TSeq<T, T>;
    function SkipWhile(const aPredicate: TPredicate<T>): TSeq<T, T>;
    function Fold<TResult>(const aFoldFunc: TFoldFunc<T, TResult>; const aInitVal: TResult): TResult;
    procedure ForEach(const aAction: TProc<T>);
  end;

  TSequence = record
  public
    class function FromString(const aString: string): TSequence<Char>; static;
    class function FromStringList(const aStrings: TStrings): TSequence<string>; static;
  end;

implementation

{ TSequence<T> }

class operator TSequence<T>.Implicit(const aArray: TArray<T>): TSequence<T>;
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

class operator TSequence<T>.Implicit(const aEnumerable: TEnumerable<T>): TSequence<T>;
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

function TSequence<T>.Filter(const aPredicate: TPredicate<T>): TSeq<T, T>;
begin
  Result := TSeq<T, T>.Create(FIterate,
    function (X: TValue<T>): TValue<T>
    begin
      if X.IsSomething and not aPredicate(X.Value) then
        Result := TValue<T>.Nothing
      else
        Result := X;
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

function TSequence<T>.Map<TResult>(const aMapper: TFunc<T, TResult>): TSeq<T, TResult>;
begin
  Result := TSeq<T, TResult>.Create(FIterate,
    function (X: TValue<T>): TValue<TResult>
    begin
      if X.IsSomething then
        Result := TValue<TResult>(aMapper(X.Value))
      else
        Result.SetState(X.State);
    end);
end;

function TSequence<T>.Skip(const aCount: Integer): TSeq<T, T>;
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

function TSequence<T>.SkipWhile(const aPredicate: TPredicate<T>): TSeq<T, T>;
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

function TSequence<T>.Take(const aCount: Integer): TSeq<T, T>;
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

function TSequence<T>.TakeWhile(const aPredicate: TPredicate<T>): TSeq<T, T>;
begin
  Result := TSeq<T, T>.Create(FIterate,
    function (X: TValue<T>): TValue<T>
    begin
      if X.IsSomething and not aPredicate(X.Value) then
        Result := TValue<T>.Stop
      else
        Result := X;
    end);
end;

{ TSequence }

class function TSequence.FromString(const aString: string): TSequence<Char>;
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

class function TSequence.FromStringList(const aStrings: TStrings): TSequence<string>;
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

end.

