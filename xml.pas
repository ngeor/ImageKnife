unit xml;

{$MODE Delphi}

interface

uses Classes, Contnrs, SysUtils;

type
  TXMLNode = class;

  TXMLNodeEnum = class
  private
    FList: TObjectList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TXMLNode;
    constructor Create(AList: TObjectList);
  public
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Item[Index: Integer]: TXMLNode read GetItem; default;
  end;

  TXMLNode = class
  private
    FName: String;
    FAttributes: TStringList;
    FChildren: TObjectList;
    function GetAttribute(const AttrName: String): String;
    procedure SetAttribute(const AttrName: String; const Value: String);
    procedure Add(Child: TXMLNode);
  public
    constructor Create;
    destructor Destroy; override;

    function AddChild: TXMLNode;

    function GetNodesByName(const str: String): TXMLNodeEnum;

    property Name: String read FName write FName;
    property Attributes[const AttrName: String]: String
      read GetAttribute write SetAttribute;
  end;

  TXMLWriter = class
  private
    FStream: TStream;
    procedure Write(const str: String); overload;
    procedure WriteLn(const str: String);
  public
    constructor Create(Stream: TStream);
    procedure Write(node: TXMLNode); overload;
  end;

  TXMLReader = class
  private
    FStream: TStream;
    FLastChar: Char;
    procedure ReadChar;
    procedure SkipSpace;
    function ReadName: String;
    function ReadValue: String;
    function IsNameCharacter: Boolean;
    function IsNameStartingCharacter: Boolean;
    function EOF: Boolean;
  public
    constructor Create(Stream: TStream);
    function Read: TXMLNode;
  end;

implementation

function EncodeXML(const str: String): String;
begin
  Result := str;
  Result := StringReplace(Result, '&', '&amp;', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
end;

function DecodeXML(const str: String): String;
begin
  Result := str;
  Result := StringReplace(Result, '&gt;', '>', [rfReplaceAll]);
  Result := StringReplace(Result, '&lt;', '<', [rfReplaceAll]);
  Result := StringReplace(Result, '&amp;', '&', [rfReplaceAll]);
end;

function TXMLNodeEnum.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TXMLNodeEnum.GetItem(Index: Integer): TXMLNode;
begin
  Result := FList[Index] as TXMLNode;
end;

constructor TXMLNodeEnum.Create(AList: TObjectList);
begin
  FList := AList;
end;

destructor TXMLNodeEnum.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;



constructor TXMLNode.Create;
begin
  FAttributes := TStringList.Create;
  FChildren := TObjectList.Create;
end;

destructor TXMLNode.Destroy;
begin
  FChildren.Free;
  FAttributes.Free;
  inherited Destroy;
end;

procedure TXMLNode.Add(Child: TXMLNode);
begin
  if Child <> nil then
    FChildren.Add(Child);
end;

function TXMLNode.AddChild: TXMLNode;
begin
  Result := TXMLNode.Create;
  FChildren.Add(Result);
end;

function TXMLNode.GetAttribute(const AttrName: String): String;
begin
  Result := FAttributes.Values[AttrName];
end;

procedure TXMLNode.SetAttribute(const AttrName: String; const Value: String);
begin
  FAttributes.Values[AttrName] := Value;
end;

function TXMLNode.GetNodesByName(const str: String): TXMLNodeEnum;
var
  list: TObjectList;
  i: Integer;
begin
  list := TObjectList.Create(False);
  for i := 0 to Self.FChildren.Count - 1 do
  begin
    if str = (FChildren[i] as TXMLNode).Name then
      list.Add(FChildren[i]);
  end;
  Result := TXMLNodeEnum.Create(list);
end;


constructor TXMLWriter.Create(Stream: TStream);
begin
  FStream := Stream;
end;

procedure TXMLWriter.Write(const str: String);
begin
  FStream.Write(str[1], Length(str));
end;

procedure TXMLWriter.WriteLn(const str: String);
begin
  Write(str + #13#10);
end;

procedure TXMLWriter.Write(Node: TXMLNode);
var
  i: Integer;
  strName, strValue: String;
begin
  Write('<' + Node.Name);
  for i := 0 to Node.FAttributes.Count - 1 do
  begin
    strName := Node.FAttributes.Names[i];
    strValue := Node.Attributes[strName];
    Write(' ' + strName + '="' + EncodeXML(strValue) + '"');
  end;

  if Node.FChildren.Count > 0 then
  begin
    WriteLn('>');

    for i := 0 to Node.FChildren.Count - 1 do
      Write(Node.FChildren[i] as TXMLNode);

    Write('</' + Node.Name + '>');
  end
  else
    WriteLn(' />');

end;

constructor TXMLReader.Create(Stream: TStream);
begin
  FStream := Stream;
  FLastChar := ' ';
end;

function TXMLReader.EOF: Boolean;
begin
  Result := FLastChar = Chr(26);
end;

procedure TXMLReader.ReadChar;
begin
  if FStream.Read(FLastChar, SizeOf(Char)) < SizeOf(Char) then
  begin
    FLastChar := Chr(26);
  end;
end;

procedure TXMLReader.SkipSpace;
begin
  while (not EOF) and (FLastChar <= ' ') do
    ReadChar;
end;

function TXMLReader.ReadName: String;
begin
  if not IsNameStartingCharacter then
    raise Exception.Create('Expected latin letter');
  Result := '';
  while (not EOF) and IsNameCharacter do
  begin
    Result := Result + FLastChar;
    ReadChar;
  end;
end;

function TXMLReader.ReadValue: String;
begin
  Result := '';
  while (not EOF) and (FLastChar <> '"') do
  begin
    Result := Result + FLastChar;
    ReadChar;
  end;
  Result := DecodeXML(Result);
end;

function TXMLReader.IsNameStartingCharacter: Boolean;
begin
  Result := ((FLastChar >= 'a') and (FLastChar <= 'z')) or
    ((FLastChar >= 'A') and (FLastChar <= 'Z'));
end;

function TXMLReader.IsNameCharacter: Boolean;
begin
  Result := (FLastChar in ['a'..'z']) or (FLastChar in ['A'..'Z']) or
    (FLastChar in ['0'..'9']) or (FLastChar in ['_', '-']);
end;


function TXMLReader.Read: TXMLNode;
var
  strAttrName, strAttrValue: String;
  closedTag: Boolean;
begin
  SkipSpace;
  if EOF then
  begin
    Result := nil;
    Exit;
  end;

  if FLastChar <> '<' then
    raise Exception.Create('Character < expected');

  ReadChar; // skip '<'

  Result := TXMLNode.Create;

  Result.Name := ReadName;

  SkipSpace;

  while (IsNameStartingCharacter) do
  begin
    strAttrName := ReadName;
    SkipSpace;
    if (FLastChar <> '=') then
      raise Exception.Create('Character = expected');
    ReadChar;
    SkipSpace;
    if (FLastChar <> '"') then
      raise Exception.Create('Character " expected');
    ReadChar;
    strAttrValue := ReadValue;
    if (FLastChar <> '"') then
      raise Exception.Create('Character " expected');
    ReadChar;
    Result.Attributes[strAttrName] := strAttrValue;

    if (EOF) then
      raise Exception.Create('Premature end of file during attribute parsing');

    SkipSpace;
  end;

  if (EOF) then
    raise Exception.Create('Premature end of file - expecting close tag');

  if (FLastChar = '/') then
  begin
    ReadChar;
    if (FLastChar <> '>') then
      raise Exception.Create('Expecting > character');
    ReadChar;

    { no children, and we succeeded!!! }
  end
  else if (FLastChar = '>') then
  begin
    ReadChar;
    closedTag := False;
    repeat
      { try to find children and then lookup for a matching </ }
      SkipSpace;
      if FLastChar <> '<' then
        raise Exception.Create('Expecting <');
      ReadChar;
      if IsNameStartingCharacter then
      begin
        // go 1 chars back
        FStream.Seek(-1, soFromCurrent);

        // set FLastChar
        FLastChar := '<';

        // read recursive
        Result.Add(Self.Read);
      end
      else if FLastChar = '/' then
      begin
        ReadChar;
        if Result.Name <> ReadName then
          raise Exception.Create('Start tag does not match close tag');
        if FLastChar <> '>' then
          raise Exception.Create('Expecting >');
        ReadChar;
        closedTag := True;
      end
      else
        raise Exception.Create('Expecting tag name or /');
    until closedTag;
  end
  else
    raise Exception.Create('Expecting / or > but found ' + FLastChar);
end;

end.
