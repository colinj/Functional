unit uRange;

interface

uses
  SysUtils, Classes,
  Generics.Collections;

type
  TIntegerRangeEnumerator = class;

  TIntegerRange = class(TEnumerable<Integer>{, IEnumerable<Integer>})
  private
    FFirst: Integer;
    FLast: Integer;
  protected
    function DoGetEnumerator: TEnumerator<Integer>; override;
  public
    constructor Create(const aFirst, aLast: Integer);
//    function GetEnumerator: IEnumerator;
//    function GenericGetEnumerator: IEnumerator<Integer>;
//    function IEnumerable<Integer>.GetEnumerator = GenericGetEnumerator;
  end;

  TIntegerRangeEnumerator = class(TEnumerator<Integer>)
  private
    FRange: TIntegerRange;
    FIndex: Integer;
//    function GetCurrent: TObject;
  protected
    function DoGetCurrent: Integer; override;
    function DoMoveNext: Boolean; override;
  public
    constructor Create(aRange: TIntegerRange);
//    function GenericGetCurrent: Integer;
//    function IEnumerator<Integer>.GetCurrent = GenericGetCurrent;
//    function  MoveNext: Boolean;
//    procedure Reset;
  end;

implementation

{ TIntegerRangeEnumerator }

constructor TIntegerRangeEnumerator.Create(aRange: TIntegerRange);
begin
  inherited Create;
  FRange := aRange;
  FIndex := FRange.FFirst - 1;
end;
{
function TIntegerRangeEnumerator.GenericGetCurrent: Integer;
begin
  Result := FIndex;
end;

function TIntegerRangeEnumerator.GetCurrent: TObject;
begin
  Result := nil;
end;

function TIntegerRangeEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FRange.FLast;
  if Result then
    Inc(FIndex);
end;

procedure TIntegerRangeEnumerator.Reset;
begin
  FIndex := FRange.FFirst - 1;
end;
}
function TIntegerRangeEnumerator.DoGetCurrent: Integer;
begin
  Result := FIndex;
end;

function TIntegerRangeEnumerator.DoMoveNext: Boolean;
begin
  Result := FIndex < FRange.FLast;
  if Result then
    Inc(FIndex);
end;

{ TIntegerRange }

constructor TIntegerRange.Create(const aFirst, aLast: Integer);
begin
  inherited Create;
  FFirst := aFirst;
  FLast := aLast;
end;

function TIntegerRange.DoGetEnumerator: TEnumerator<Integer>;
begin
  Result := TIntegerRangeEnumerator.Create(Self);
end;
{
function TIntegerRange.GenericGetEnumerator: IEnumerator<Integer>;
begin
  Result := TIntegerRangeEnumerator.Create(Self);
end;

function TIntegerRange.GetEnumerator: IEnumerator;
begin
  Result := TIntegerRangeEnumerator.Create(Self);
end;
}
end.
