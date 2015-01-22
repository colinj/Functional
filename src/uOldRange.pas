unit uOldRange;

interface

uses
  SysUtils, Classes;

type
  TValue<T> = record
    Ignore: Boolean;
    Value: T;
  public
    constructor Create(aIgnore: Boolean; aValue: T);
  end;

  TIntFunc = TFunc<Integer, Integer>;
  TIntPredicate = TPredicate<Integer>;

  TResultFunc<T> = reference to function(X: TValue<T>): TValue<T>;

  TRange = class(TObject)
  private
    FStart: Integer;
    FEnd: Integer;
    FFunc: TResultFunc<Integer>;
  public
    constructor Create(const aStart, aEnd: Integer);
    function Map(const aFunc: TIntFunc): TRange;
    function Filter(const aFunc: TIntPredicate): TRange;
    procedure DoIt(const aAction: TProc<Integer>);
    procedure SumTo(out aValue: Integer);
  end;

implementation

{ TResult }

constructor TValue<T>.Create(aIgnore: Boolean; aValue: T);
begin
  Ignore := aIgnore;
  Value := aValue;
end;

{ TRange }

constructor TRange.Create(const aStart, aEnd: Integer);
begin
  FStart := aStart;
  FEnd := aEnd;
  FFunc :=
    function (X: TValue<Integer>): TValue<Integer>
    begin
      Result := X;
    end;
end;

procedure TRange.DoIt(const aAction: TProc<Integer>);
var
  I: Integer;
  R: TValue<Integer>;
begin
  for I := FStart to FEnd do
  begin
    R := FFunc(TValue<Integer>.Create(False, I));
    if not R.Ignore then
      aAction(R.Value);
  end;
end;

function TRange.Filter(const aFunc: TIntPredicate): TRange;
var
  OldFunc: TResultFunc<Integer>;
begin
  OldFunc := FFunc;
  FFunc :=
    function (X: TValue<Integer>): TValue<Integer>
    var
      R: TValue<Integer>;
    begin
      R := OldFunc(X);
      if R.Ignore then
        Result := R
      else
        Result := TValue<Integer>.Create(not aFunc(R.Value), R.Value);
    end;
  Result := Self;
end;

function TRange.Map(const aFunc: TIntFunc): TRange;
var
  OldFunc: TResultFunc<Integer>;
begin
  OldFunc := FFunc;
  FFunc :=
    function (X: TValue<Integer>): TValue<Integer>
    var
      R: TValue<Integer>;
    begin
      R := OldFunc(X);
      if R.Ignore then
        Result := R
      else
        Result := TValue<Integer>.Create(False, aFunc(R.Value));
    end;
  Result := Self;
end;

procedure TRange.SumTo(out aValue: Integer);
var
  I: Integer;
  R: TValue<Integer>;
begin
  aValue := 0;

  for I := FStart to FEnd do
  begin
    R := FFunc(TValue<Integer>.Create(False, I));
    if not R.Ignore then
      Inc(aValue, R.Value);
  end;
end;

end.
