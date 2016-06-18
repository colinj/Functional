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

unit Functional.Value;

interface

uses
  SysUtils, Classes;

type
  TValueState = (vsStart, vsSomething, vsNothing, vsFinish);

  TValue<T> = record
  private
    FValue: T;
    FState: TValueState;
  public
    class operator Implicit(const aValue: T): TValue<T>;
    class function Nothing: TValue<T>; static;
    class function Start: TValue<T>; static;
    class function Finish: TValue<T>; static;
    function HasValue: Boolean;
    procedure SetState(const aState: TValueState);
    property Value: T read FValue;
    property State: TValueState read FState;
  end;

implementation

{ TValue<T> }

class operator TValue<T>.Implicit(const aValue: T): TValue<T>;
begin
  Result.FValue := aValue;
  Result.FState := vsSomething;
end;

function TValue<T>.HasValue: Boolean;
begin
  Result := FState = vsSomething;
end;

class function TValue<T>.Nothing: TValue<T>;
begin
  Result.FState := vsNothing;
end;

procedure TValue<T>.SetState(const aState: TValueState);
begin
  FState := aState;
end;

class function TValue<T>.Start: TValue<T>;
begin
  Result.FState := vsStart;
end;

class function TValue<T>.Finish: TValue<T>;
begin
  Result.FState := vsFinish;
end;

end.
