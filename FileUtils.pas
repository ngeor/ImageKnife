unit FileUtils;

{$MODE Delphi}

interface

uses LCLIntf, LCLType, LMessages, Types;

function ReadString(var f: file): string;
procedure WriteString(var f: file; const s: string);

function ReadInt(var f: file): integer;
procedure WriteInt(var f: file; const i: integer);

function ReadRect(var f: file): TRect;
procedure WriteRect(var f: file; const Rect: TRect);

implementation

function ReadString(var f: file): string;
var
  len: integer;
begin
  BlockRead(f, len, SizeOf(len));
  SetLength(Result, len);
  if (len > 0) then
    BlockRead(f, Result[1], len);
end;

procedure WriteString(var f: file; const s: string);
var
  len: integer;
begin
  len := Length(s);
  BlockWrite(f, len, sizeof(len));
  if (len > 0) then
    BlockWrite(f, s[1], len);
end;

function ReadInt(var f: file): integer;
begin
  BlockRead(f, Result, SizeOf(Result));
end;

procedure WriteInt(var f: file; const i: integer);
begin
  BlockWrite(f, i, sizeof(i));
end;

function ReadRect(var f: file): TRect;
begin
  BlockRead(f, Result, SizeOf(Result));
end;

procedure WriteRect(var f: file; const Rect: TRect);
begin
  BlockWrite(f, Rect, sizeOf(Rect));
end;

end.
