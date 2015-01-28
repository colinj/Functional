unit uRange;

interface

uses
  SysUtils, Classes,
  Generics.Collections;

type
  TIntegerRangeEnumerator = class;

  TIntegerRange = class(TEnumerable<Integer>)
  private
    FFirst: Integer;
    FLast: Integer;
  protected
    function DoGetEnumerator: TEnumerator<Integer>; override;
  public
    constructor Create(const aFirst, aLast: Integer);
  end;

  TIntegerRangeEnumerator = class(TEnumerator<Integer>)
  private
    FRange: TIntegerRange;
    FIndex: Integer;
  protected
    function DoGetCurrent: Integer; override;
    function DoMoveNext: Boolean; override;
  public
    constructor Create(aRange: TIntegerRange);
  end;

  TIntegerStreamEnumerator = class;

  TIntegerStream = class(TEnumerable<Integer>)
  private
    FStart: Integer;
  protected
    function DoGetEnumerator: TEnumerator<Integer>; override;
  public
    constructor Create(const aStart: Integer = 0);
  end;

  TIntegerStreamEnumerator = class(TEnumerator<Integer>)
  private
    FStream: TIntegerStream;
    FIndex: Integer;
  protected
    function DoGetCurrent: Integer; override;
    function DoMoveNext: Boolean; override;
  public
    constructor Create(aStream: TIntegerStream);
  end;

implementation

{ TIntegerRangeEnumerator }

constructor TIntegerRangeEnumerator.Create(aRange: TIntegerRange);
begin
  inherited Create;
  FRange := aRange;
  FIndex := FRange.FFirst - 1;
end;

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

{ TIntegerStream }

constructor TIntegerStream.Create(const aStart: Integer);
begin
  inherited Create;
  FStart := aStart;
end;

function TIntegerStream.DoGetEnumerator: TEnumerator<Integer>;
begin
  Result := TIntegerStreamEnumerator.Create(Self);
end;

{ TIntegerStreamEnumerator }

constructor TIntegerStreamEnumerator.Create(aStream: TIntegerStream);
begin
  inherited Create;
  FStream := aStream;
  FIndex := FStream.FStart - 1;
end;

function TIntegerStreamEnumerator.DoGetCurrent: Integer;
begin
  Result := FIndex;
end;

function TIntegerStreamEnumerator.DoMoveNext: Boolean;
begin
  Result := FIndex < MaxInt;
  if Result then
    Inc(FIndex);
end;

end.
