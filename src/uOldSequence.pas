unit uOldSequence;

interface

uses
  SysUtils, Classes;

type
  TValue<T> = record
    Value: T;
    IsValid: Boolean;
  public
    constructor Create(const aValue: T; const aValidFlag: Boolean = True);
  end;

  TValueFunc<T, U> = reference to function(X: TValue<T>): TValue<U>;

  TSequence = class(TObject)
  private
    FStart: Integer;
    FEnd: Integer;
    FFunc: TValueFunc<Integer, Integer>;
  public
    constructor Create(const aStart, aEnd: Integer);
    function Map(const aFunc: TFunc<Integer, Integer>): TSequence;
    function Filter(const aPredicate: TPredicate<Integer>): TSequence;
    procedure DoIt(const aAction: TProc<Integer>);
    procedure SumTo(out aValue: Integer);
  end;

implementation

{ TValue<T> }

constructor TValue<T>.Create(const aValue: T; const aValidFlag: Boolean);
begin
  Value := aValue;
  IsValid := aValidFlag;
end;

{ TSequence }

constructor TSequence.Create(const aStart, aEnd: Integer);
begin
  FStart := aStart;
  FEnd := aEnd;
  FFunc :=
    function (X: TValue<Integer>): TValue<Integer>
    begin
      Result := X;
    end;
end;

procedure TSequence.DoIt(const aAction: TProc<Integer>);
var
  I: Integer;
  R: TValue<Integer>;
begin
  for I := FStart to FEnd do
  begin
    R := FFunc(TValue<Integer>.Create(I));
    if R.IsValid then
      aAction(R.Value);
  end;
end;

function TSequence.Filter(const aPredicate: TPredicate<Integer>): TSequence;
var
  OldFunc: TValueFunc<Integer, Integer>;
begin
  OldFunc := FFunc;
  FFunc :=
    function (X: TValue<Integer>): TValue<Integer>
    var
      R: TValue<Integer>;
    begin
      R := OldFunc(X);
      if R.IsValid then
        Result := TValue<Integer>.Create(R.Value, aPredicate(R.Value))
      else
        Result := R;
    end;
  Result := Self;
end;

function TSequence.Map(const aFunc: TFunc<Integer, Integer>): TSequence;
var
  OldFunc: TValueFunc<Integer, Integer>;
begin
  OldFunc := FFunc;
  FFunc :=
    function (X: TValue<Integer>): TValue<Integer>
    var
      R: TValue<Integer>;
    begin
      R := OldFunc(X);
      if R.IsValid then
        Result := TValue<Integer>.Create(aFunc(R.Value))
      else
        Result := R;
    end;
  Result := Self;
end;

procedure TSequence.SumTo(out aValue: Integer);
var
  I: Integer;
  R: TValue<Integer>;
begin
  aValue := 0;

  for I := FStart to FEnd do
  begin
    R := FFunc(TValue<Integer>.Create(I));
    if R.IsValid then
      Inc(aValue, R.Value);
  end;
end;

end.

