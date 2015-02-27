{****************************************************}
{                                                    }
{  FunctionalLib                                     }
{                                                    }
{  Copyright (C) 2015 Colin Johnsun                  }
{                                                    }
{  https:/github.com/colinj                          }
{                                                    }
{****************************************************}
{                                                    }
{  This Source Code Form is subject to the terms of  }
{  the Mozilla Public License, v. 2.0. If a copy of  }
{  the MPL was not distributed with this file, You   }
{  can obtain one at                                 }
{                                                    }
{  http://mozilla.org/MPL/2.0/                       }
{                                                    }
{****************************************************}

unit uSequenceFunctions;

interface

uses
  SysUtils, Classes,
  Generics.Collections,
  uValue;

type
  TValueFunc<T, U> = reference to function (Item: TValue<T>): TValue<U>;
  TIteratorProc<T> = reference to procedure (P: TPredicate<T>);
  TFoldFunc<T, U> = reference to function (Item: T; Acc: U): U;

  TSeqFunction<T, U> = record
  public
    class function CreateFilter(const aPrevFunc: TValueFunc<T, U>; const aPredicate: TPredicate<U>): TValueFunc<T, U>; static;
    class function CreateMap<TResult>(const aPrevFunc: TValueFunc<T, U>; const aMapper: TFunc<U, TResult>): TValueFunc<T, TResult>; static;
    class function CreateTake(const aPrevFunc: TValueFunc<T, U>; const aCount: Integer): TValueFunc<T, U>; static;
    class function CreateSkip(const aPrevFunc: TValueFunc<T, U>; const aCount: Integer): TValueFunc<T, U>; static;
    class function CreateTakeWhile(const aPrevFunc: TValueFunc<T, U>; const aPredicate: TPredicate<U>): TValueFunc<T, U>; static;
    class function CreateSkipWhile(const aPrevFunc: TValueFunc<T, U>; const aPredicate: TPredicate<U>): TValueFunc<T, U>; static;
    class function CreateFold<TResult>(const aPrevFunc: TValueFunc<T, U>; const aFoldFunc: TFoldFunc<U, TResult>;
      const aInitVal: TResult; const aCaptureVal: TProc<TResult>): TPredicate<T>; static;
    class function CreateAction(const aPrevFunc: TValueFunc<T, U>; const  aAction: TProc<U>): TPredicate<T>; static;
    class function CreateAddItem(const aPrevFunc: TValueFunc<T, U>; const aList: TList<U>): TPredicate<T>; static;
  end;

implementation

class function TSeqFunction<T, U>.CreateAction(const aPrevFunc: TValueFunc<T, U>; const aAction: TProc<U>): TPredicate<T>;
begin
  Result :=
    function (Item: T): Boolean
    var
      R: TValue<U>;
    begin
      Result := True;
      R := aPrevFunc(TValue<T>(Item));
      case R.State of
        vsSomething: aAction(R.Value);
        vsFinish: Result := False;
      end;
    end;
end;

class function TSeqFunction<T, U>.CreateAddItem(const aPrevFunc: TValueFunc<T, U>; const aList: TList<U>): TPredicate<T>;
var
  ItemList: TList<U>;
begin
  ItemList := aList;

  Result :=
    function (Item: T): Boolean
    var
      R: TValue<U>;
    begin
      R := aPrevFunc(TValue<T>(Item));
      if R.IsSomething then
        ItemList.Add(R.Value);
      Result := R.State <> vsFinish;
    end;
end;

class function TSeqFunction<T, U>.CreateFilter(const aPrevFunc: TValueFunc<T, U>; const aPredicate: TPredicate<U>): TValueFunc<T, U>;
begin
  Result :=
    function (Item: TValue<T>): TValue<U>
    begin
      Result := aPrevFunc(Item);
      if Result.IsSomething and not aPredicate(Result.Value) then
        Result := TValue<U>.Nothing;
    end;
end;

class function TSeqFunction<T, U>.CreateMap<TResult>(const aPrevFunc: TValueFunc<T, U>; const aMapper: TFunc<U, TResult>): TValueFunc<T, TResult>;
begin
  Result :=
    function (Item: TValue<T>): TValue<TResult>
    var
      R: TValue<U>;
    begin
      R := aPrevFunc(Item);
      if R.IsSomething then
        Result := TValue<TResult>(aMapper(R.Value))
      else
        Result.SetState(R.State);
    end;
end;

class function TSeqFunction<T, U>.CreateSkip(const aPrevFunc: TValueFunc<T, U>; const aCount: Integer): TValueFunc<T, U>;
var
  Counter: Integer;
begin
  Result :=
    function (Item: TValue<T>): TValue<U>
    begin
      Result := aPrevFunc(Item);
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

class function TSeqFunction<T, U>.CreateSkipWhile(const aPrevFunc: TValueFunc<T, U>; const aPredicate: TPredicate<U>): TValueFunc<T, U>;
var
  Skipping: Boolean;
begin
  Result :=
    function (Item: TValue<T>): TValue<U>
    begin
      Result := aPrevFunc(Item);

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
    end;
end;

class function TSeqFunction<T, U>.CreateTake(const aPrevFunc: TValueFunc<T, U>; const aCount: Integer): TValueFunc<T, U>;
var
  Counter: Integer;
begin
  Result :=
    function (Item: TValue<T>): TValue<U>
    begin
      if Counter = aCount then
        begin
          Result := TValue<U>.Finish;
          Exit;
        end;

      Result := aPrevFunc(Item);
      case Result.State of
        vsStart: Counter := 0;
        vsSomething: Inc(Counter);
      end;
    end;
end;

class function TSeqFunction<T, U>.CreateTakeWhile(const aPrevFunc: TValueFunc<T, U>; const aPredicate: TPredicate<U>): TValueFunc<T, U>;
begin
  Result :=
    function (Item: TValue<T>): TValue<U>
    begin
      Result := aPrevFunc(Item);
      if Result.IsSomething and not aPredicate(Result.Value) then
        Result := TValue<U>.Finish;
    end;
end;

class function TSeqFunction<T, U>.CreateFold<TResult>(const aPrevFunc: TValueFunc<T, U>; const aFoldFunc: TFoldFunc<U, TResult>;
  const aInitVal: TResult; const aCaptureVal: TProc<TResult>): TPredicate<T>;
var
  Accumulator: TResult;
begin
  Accumulator := aInitVal;

  Result :=
    function (Item: T): Boolean
    var
      R: TValue<U>;
    begin
      Result := True;
      R := aPrevFunc(TValue<T>(Item));
      case R.State of
        vsSomething: Accumulator := aFoldFunc(R.Value, Accumulator);
        vsFinish:
          begin
            aCaptureVal(Accumulator);
            Result := False;
          end;
      end;
    end;
end;

end.
