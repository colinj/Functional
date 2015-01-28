unit uCounter;

interface

uses SysUtils, Classes;


function StaticCounter: Integer;
function CreateCounter: TFunc<Integer>;

function IsPrime(N: Integer): Boolean;

implementation

var
  StaticIndex: Integer = 0;

function StaticCounter: Integer;
begin
  Inc(StaticIndex); // StaticIndex := StaticIndex + 1;
  Result := StaticIndex;
end;

function CreateCounter: TFunc<Integer>;
var
  Index: Integer;
begin
  Index := 0;

  Result :=
    function (): Integer
    begin
      Inc(Index);
      Result := Index;
    end;
end;

function IsPrime(N: Integer): Boolean;
var
 Test: Integer;
 M: Integer ;
begin
  M := Trunc(Sqrt(N));

  IsPrime := True;
  for Test := 2 to N - 1 do
    if (N mod Test) = 0 then
    begin
      IsPrime := False;
      Break; {jump out of the for loop}
    end;
end;

end.
