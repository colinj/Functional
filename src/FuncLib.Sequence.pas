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

unit FuncLib.Sequence;

interface

uses
  SysUtils, Classes,
  Generics.Collections,
  DB,
  FuncLib.Value,
  FuncLib.SequenceFunctions;

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
    class function Identity<T>(Item: TValue<T>): TValue<T>; static;
    class function From<T>(const aArray: TArray<T>): TSequence<T, T>; overload; static;
    class function From<T>(const aEnumerable: TEnumerable<T>): TSequence<T, T>; overload; static;
    class function From(const aString: string): TSequence<Char, Char>; overload; static;
    class function From(const aStrings: TStrings): TSequence<string, string>; overload; static;
    class function From(const aDataset: TDataSet): TSequence<TDataSet, TDataSet>; overload; static;
    class function Range(const aStart, aFinish: Integer): TSequence<Integer, Integer>; static;
  end;
//
//  TSequence<Int, U> = record
//  private
//    FFunc: TValueFunc<Int, U>;
//    FIterate: TIteratorProc<Int>;
//  public
//    constructor Create(const aIterator: TIteratorProc<Int>; const aFunc: TValueFunc<Int, U>);
//    function Filter(const aPredicate: TPredicate<U>): TSequence<Int, U>;
////    function Map<TResult>(const aMapper: TFunc<U, TResult>): TSequence<Int, TResult>;
////    function Take(const aCount: Integer): TSequence<Int, U>;
////    function Skip(const aCount: Integer): TSequence<Int, U>;
////    function TakeWhile(const aPredicate: TPredicate<U>): TSequence<Int, U>;
////    function SkipWhile(const aPredicate: TPredicate<U>): TSequence<Int, U>;
//    function Fold<TResult>(const aFoldFunc: TFoldFunc<U, TResult>; const aInitVal: TResult): TResult;
////    function ToList: TList<U>;
//    procedure ForEach(const aAction: TProc<U>);
//  end;

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
  OrigFunc: TValueFunc<T, U>;
  Accumulator: TResult;
begin
  OrigFunc := FFunc;

  Accumulator := aInitVal;

  FFunc(TValue<T>.Start);
  FIterate(
    function (Item: T): Boolean
    var
      R: TValue<U>;
    begin
      Result := False;
      R := OrigFunc(TValue<T>(Item));
      case R.State of
        vsSomething: Accumulator := aFoldFunc(R.Value, Accumulator);
        vsFinish: Result := True;
      end;
    end
  );

  Result := Accumulator;
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

class function TSequence.Identity<T>(Item: TValue<T>): TValue<T>;
begin
  Result := Item;
end;

class function TSequence.From<T>(const aArray: TArray<T>): TSequence<T, T>;
begin
  Result := TSequence<T, T>.Create(
    procedure (StopOn: TPredicate<T>)
    var
      Item: T;
    begin
      for Item in aArray do
        if StopOn(Item) then Break;
    end,
    Identity<T>);
end;

class function TSequence.From<T>(const aEnumerable: TEnumerable<T>): TSequence<T, T>;
begin
  Result := TSequence<T, T>.Create(
    procedure (StopOn: TPredicate<T>)
    var
      Item: T;
    begin
      for Item in aEnumerable do
        if StopOn(Item) then Break;
    end,
    Identity<T>);
end;

class function TSequence.From(const aString: string): TSequence<Char, Char>;
begin
  Result := TSequence<Char, Char>.Create(
    procedure (StopOn: TPredicate<Char>)
    var
      Item: Char;
    begin
      for Item in aString do
        if StopOn(Item) then Break;
    end,
    Identity<Char>);
end;

class function TSequence.From(const aStrings: TStrings): TSequence<string, string>;
begin
  Result := TSequence<string, string>.Create(
    procedure (StopOn: TPredicate<string>)
    var
      Item: string;
    begin
      for Item in aStrings do
        if StopOn(Item) then Break;
    end,
    Identity<string>);
end;

class function TSequence.From(const aDataset: TDataSet): TSequence<TDataSet, TDataSet>;
begin
  Result := TSequence<TDataSet, TDataSet>.Create(
    procedure (StopOn: TPredicate<TDataSet>)
    var
      D: TDataSet;
    begin
      D := aDataset;
      D.First;
      while not D.Eof do
      begin
        if StopOn(D) then Break;
        D.Next;
      end;
    end,
    Identity<TDataSet>);
end;

class function TSequence.Range(const aStart, aFinish: Integer): TSequence<Integer, Integer>;
begin
  Result := TSequence<Integer, Integer>.Create(
    procedure (StopOn: TPredicate<Integer>)
    var
      Item: Integer;
    begin
      for Item := aStart to aFinish do
        if StopOn(Item) then Break;
    end,
    Identity<Integer>);
end;

end.

