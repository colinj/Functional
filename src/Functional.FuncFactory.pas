{****************************************************}
{                                                    }
{  Delphi Functional Library                         }
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

unit Functional.FuncFactory;

interface

uses
  SysUtils, Classes,
  Generics.Collections,
  Functional.Value;

type
  TPredicate<T> = reference to function (const Arg1: T): Boolean;
  TValueFunc<T, U> = reference to function (const Item: TValue<T>): TValue<U>;
  TIteratorProc<T> = reference to procedure (const P: TPredicate<T>);
  TFoldFunc<T, U> = reference to function (const Item: T; const Acc: U): U;

  TFuncFactory<T, U> = record
  public
    class function CreateFilter(const aFunc: TValueFunc<T, U>; const aPredicate: TPredicate<U>): TValueFunc<T, U>; static;
    class function CreateMap<TResult>(const aFunc: TValueFunc<T, U>; const aMapper: TFunc<U, TResult>): TValueFunc<T, TResult>; static;
    class function CreateTake(const aFunc: TValueFunc<T, U>; const aCount: Integer): TValueFunc<T, U>; static;
    class function CreateSkip(const aFunc: TValueFunc<T, U>; const aCount: Integer): TValueFunc<T, U>; static;
    class function CreateTakeWhile(const aFunc: TValueFunc<T, U>; const aPredicate: TPredicate<U>): TValueFunc<T, U>; static;
    class function CreateSkipWhile(const aFunc: TValueFunc<T, U>; const aPredicate: TPredicate<U>): TValueFunc<T, U>; static;

    class function ForEach(const aFunc: TValueFunc<T, U>; const  aAction: TProc<U>): TPredicate<T>; static;
    class function AddItem(const aFunc: TValueFunc<T, U>; const aList: TList<U>): TPredicate<T>; static;
  end;

implementation

class function TFuncFactory<T, U>.ForEach(const aFunc: TValueFunc<T, U>; const aAction: TProc<U>): TPredicate<T>;
begin
  Result :=
    function (const Item: T): Boolean
    var
      R: TValue<U>;
    begin
      Result := False;
      R := aFunc(TValue<T>(Item));
      case R.State of
        vsSomething: aAction(R.Value);
        vsFinish: Result := True;
      end;
    end;
end;

class function TFuncFactory<T, U>.AddItem(const aFunc: TValueFunc<T, U>; const aList: TList<U>): TPredicate<T>;
var
  ItemList: TList<U>;
begin
  ItemList := aList;

  Result :=
    function (const Item: T): Boolean
    var
      R: TValue<U>;
    begin
      R := aFunc(TValue<T>(Item));
      if R.HasValue then
        ItemList.Add(R.Value);
      Result := R.State = vsFinish;
    end;
end;

class function TFuncFactory<T, U>.CreateFilter(const aFunc: TValueFunc<T, U>; const aPredicate: TPredicate<U>): TValueFunc<T, U>;
begin
  Result :=
    function (const Item: TValue<T>): TValue<U>
    begin
      Result := aFunc(Item);
      if Result.HasValue and not aPredicate(Result.Value) then
        Result := TValue<U>.Nothing;
    end;
end;

class function TFuncFactory<T, U>.CreateMap<TResult>(const aFunc: TValueFunc<T, U>; const aMapper: TFunc<U, TResult>): TValueFunc<T, TResult>;
begin
  Result :=
    function (const Item: TValue<T>): TValue<TResult>
    var
      R: TValue<U>;
    begin
      R := aFunc(Item);
      if R.HasValue then
        Result := TValue<TResult>(aMapper(R.Value))
      else
        Result.SetState(R.State);
    end;
end;

class function TFuncFactory<T, U>.CreateSkip(const aFunc: TValueFunc<T, U>; const aCount: Integer): TValueFunc<T, U>;
var
  Counter: Integer;
begin
  Result :=
    function (const Item: TValue<T>): TValue<U>
    begin
      Result := aFunc(Item);
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

class function TFuncFactory<T, U>.CreateSkipWhile(const aFunc: TValueFunc<T, U>; const aPredicate: TPredicate<U>): TValueFunc<T, U>;
var
  Skipping: Boolean;
begin
  Result :=
    function (const Item: TValue<T>): TValue<U>
    begin
      Result := aFunc(Item);

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

class function TFuncFactory<T, U>.CreateTake(const aFunc: TValueFunc<T, U>; const aCount: Integer): TValueFunc<T, U>;
var
  Counter: Integer;
begin
  Result :=
    function (const Item: TValue<T>): TValue<U>
    begin
      if Counter = aCount then
        begin
          Result := TValue<U>.Finish;
          Exit;
        end;

      Result := aFunc(Item);
      case Result.State of
        vsStart: Counter := 0;
        vsSomething: Inc(Counter);
      end;
    end;
end;

class function TFuncFactory<T, U>.CreateTakeWhile(const aFunc: TValueFunc<T, U>; const aPredicate: TPredicate<U>): TValueFunc<T, U>;
begin
  Result :=
    function (const Item: TValue<T>): TValue<U>
    begin
      Result := aFunc(Item);
      if Result.HasValue and not aPredicate(Result.Value) then
        Result := TValue<U>.Finish;
    end;
end;

end.
