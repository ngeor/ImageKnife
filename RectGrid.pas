unit RectGrid;

{$MODE Delphi}

interface

uses SysUtils, Classes, LCLIntf, LCLType, LMessages, Contnrs, xml;

type
  TSliceType = (stNone, stHorizontal, stVertical);

  TRectGrid = class;

  TRectGridList = class
  private
    FList: TObjectList;
    function GetItem(Index: Integer): TRectGrid;
    function GetCount: Integer;
  protected
    procedure Remove(rg: TRectGrid);
    procedure Insert(Index: Integer; rg: TRectGrid);
    procedure Add(rg: TRectGrid);

  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function IndexOf(rg: TRectGrid): Integer;




    property Items[Index: Integer]: TRectGrid read GetItem; default;
    property Count: Integer read GetCount;

  end;

  TRectGrid = class
  private
    FRect: TRect;
    FKids: TRectGridList;
    FName: String;

    FSliceType: TSliceType;
    FParent: TRectGrid;
    FSingleColor: DWORD;
    FIsSingleColor: Boolean;
    FIsBackground: Boolean;
    FAttributes: TStringList;
    function GetAttribute(const AttrName: String): String;
    procedure SetAttribute(const AttrName: String; const Value: String);
    function IsSliced: Boolean;
    function NewKid(const rt: TRect): TRectGrid;
  public
    constructor Create(const rt: TRect);
    destructor Destroy; override;
    function RectFromPos(const Pt: TPoint): TRectGrid;
    procedure RectsFromX(const X: Integer; list: TObjectList);
    procedure RectsFromY(const Y: Integer; list: TObjectList);
    procedure SliceHorizontal(const Y: Integer);
    procedure SliceVertical(const X: Integer);
    procedure GroupKids(FromIndex, ToIndex: Integer);

    procedure LoadFromXML(me: TXMLNode);
    procedure SaveToXML(parent: TXMLNode);
    property Parent: TRectGrid read FParent;
    property Rect: TRect read FRect;

    property Kids: TRectGridList read FKids;
    property Sliced: Boolean read IsSliced;
    property SliceType: TSliceType read FSliceType;
    property Name: String read FName write FName;

    property IsSingleColor: Boolean read FIsSingleColor write FIsSingleColor;
    property SingleColor: DWORD read FSingleColor write FSingleColor;

    property IsBackground: Boolean read FIsBackground write FIsBackground;

    property Attributes[const AttrName: String]: String
      read GetAttribute write SetAttribute;
  end;

implementation

uses FileUtils;

constructor TRectGridList.Create;
begin
  FList := TObjectList.Create;
end;

destructor TRectGridList.Destroy;
begin
  FList.Free;
end;

function TRectGridList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TRectGridList.GetItem(Index: Integer): TRectGrid;
begin
  Result := TRectGrid(FList.Items[Index]);
end;

function TRectGridList.IndexOf(rg: TRectGrid): Integer;
begin
  Result := FList.IndexOf(rg);
end;

procedure TRectGridList.Insert(Index: Integer; rg: TRectGrid);
begin
  FList.Insert(Index, rg);
end;

procedure TRectGridList.Add(rg: TRectGrid);
begin
  FList.Add(rg);
end;

procedure TRectGridList.Remove(rg: TRectGrid);
begin
  FList.Remove(rg);
end;

procedure TRectGridList.Clear;
begin
  FList.Clear;
end;

constructor TRectGrid.Create(const rt: TRect);
begin
  Self.FRect := rt;
  FKids := TRectGridList.Create;
  FSliceType := stNone;
  FParent := nil;
  FAttributes := TStringList.Create;
end;

destructor TRectGrid.Destroy;
begin
  if FParent <> nil then
    FParent.FKids.Remove(Self);
  FKids.Free;
  FAttributes.Free;
  inherited Destroy;
end;

function TRectGrid.GetAttribute(const AttrName: String): String;
begin
  Result := FAttributes.Values[AttrName];
end;

procedure TRectGrid.SetAttribute(const AttrName: String; const Value: String);
begin
  FAttributes.Values[AttrName] := Value;
end;

function TRectGrid.IsSliced: Boolean;
begin
  Result := FKids.Count >= 2;
end;

function TRectGrid.RectFromPos(const Pt: TPoint): TRectGrid;
var
  i: Integer;
begin
  Result := nil;
  if PtInRect(FRect, pt) then
  begin
    if Sliced then
    begin
      for i := 0 to FKids.Count - 1 do
      begin
        Result := TRectGrid(FKids[i]).RectFromPos(pt);
        if Result <> nil then
          Break;
      end;
    end
    else
    begin
      Result := Self;
    end;
  end;
end;

procedure TRectGrid.RectsFromX(const X: Integer; list: TObjectList);
var
  i: Integer;
begin
  if (FRect.Left <= X) and (X <= FRect.Right) then
    if (not Sliced) then
      list.Add(Self)
    else
    begin
      for i := 0 to FKids.Count - 1 do
        TRectGrid(FKids[i]).RectsFromX(X, list);
    end;
end;

procedure TRectGrid.RectsFromY(const Y: Integer; list: TObjectList);
var
  i: Integer;
begin
  if (FRect.Top <= Y) and (Y <= FRect.Bottom) then
    if (not Sliced) then
      list.Add(Self)
    else
    begin
      for i := 0 to FKids.Count - 1 do
        TRectGrid(FKids[i]).RectsFromY(Y, list);
    end;
end;

function TRectGrid.NewKid(const rt: TRect): TRectGrid;
begin
  Result := TRectGrid.Create(rt);
  Result.FParent := Self;
end;

procedure TRectGrid.SliceHorizontal(const Y: Integer);
var
  parentIndex: Integer;
  rtA, rtB: TRect;
begin
  if (FRect.Top > Y) or (Y > FRect.Bottom) then
    raise Exception.Create('Invalid Y coordinate');
  if (Sliced) then
    raise Exception.Create('Already sliced');

  rtA := Classes.Rect(FRect.Left, FRect.Top, FRect.Right, Y);
  rtB := Classes.Rect(FRect.Left, Y, FRect.Right, FRect.Bottom);

  if (FParent <> nil) and (FParent.FSliceType = stHorizontal) then
  begin
    parentIndex := FParent.FKids.IndexOf(Self);
    if parentIndex = -1 then
      raise Exception.Create('Kid was not found in parent list');

    FParent.FKids.Insert(parentIndex, FParent.NewKid(rtA));
    FRect := rtB;
  end
  else
  begin
    FKids.Add(NewKid(rtA));
    FKids.Add(NewKid(rtB));
    FSliceType := stHorizontal;
  end;

end;

procedure TRectGrid.SliceVertical(const X: Integer);
var
  parentIndex: Integer;
  rtA, rtB: TRect;
begin
  if (FRect.Left > X) or (X > FRect.Right) then
    raise Exception.Create('Invalid X coordinate');
  if (Sliced) then
    raise Exception.Create('Already sliced');

  rtA := Classes.Rect(FRect.Left, FRect.Top, X, FRect.Bottom);
  rtB := Classes.Rect(X, FRect.Top, FRect.Right, FRect.Bottom);

  if (FParent <> nil) and (FParent.FSliceType = stVertical) then
  begin
    parentIndex := FParent.FKids.IndexOf(Self);
    if parentIndex = -1 then
      raise Exception.Create('Kid was not found in parent list');

    FParent.FKids.Insert(parentIndex, FParent.NewKid(rtA));
    FRect := rtB;
  end
  else
  begin
    FKids.Add(NewKid(rtA));
    FKids.Add(NewKid(rtB));
    FSliceType := stVertical;
  end;
end;

procedure TRectGrid.LoadFromXML(me: TXMLNode);
const
  strSlice: array [TSliceType] of String = ('stNone', 'stHorizontal', 'stVertical');
var
  i: Integer;
  rg: TRectGrid;
  tmp: String;
  enum: TXMLNodeEnum;
  strName, strValue: String;
begin

  Self.Name := me.Attributes['Name'];

  tmp := me.Attributes['SliceType'];
  if (tmp = 'stNone') then
    FSliceType := stNone
  else if (tmp = 'stHorizontal') then
    FSliceType := stHorizontal
  else if (tmp = 'stVertical') then
    FSliceType := stVertical
  else
    raise Exception.Create('Invalid slice type ' + tmp + ' was specified');

  FRect.Left := StrToInt(me.Attributes['Left']);
  FRect.Top := StrToInt(me.Attributes['Top']);
  FRect.Right := StrToInt(me.Attributes['Right']);
  FRect.Bottom := StrToInt(me.Attributes['Bottom']);

  FIsBackground := me.Attributes['IsBackground'] = 'true';

  tmp := me.Attributes['SingleColor'];
  if tmp <> '' then
  begin
    FSingleColor := StrToInt(tmp);
    FIsSingleColor := True;
  end
  else
    FIsSingleColor := False;

  FKids.Clear;

  enum := me.GetNodesByName('attr');
  for i := 0 to enum.Count - 1 do
  begin
    strName := enum[i].Attributes['name'];
    strValue := enum[i].Attributes['value'];
    Self.Attributes[strName] := strValue;
  end;
  enum.Free;

  enum := me.GetNodesByName('cell');
  for i := 0 to enum.Count - 1 do
  begin
    rg := NewKid(Classes.Rect(0, 0, 0, 0));
    rg.LoadFromXML(enum[i]);
    FKids.Add(rg);
  end;
  enum.Free;

end;

procedure TRectGrid.SaveToXML(parent: TXMLNode);
const
  strSlice: array [TSliceType] of String = ('stNone', 'stHorizontal', 'stVertical');
var
  i: Integer;
  me, attrNode: TXMLNode;
  strName, strValue: String;
begin
  me := parent.AddChild;
  me.Name := 'cell';
  me.Attributes['Name'] := FName;
  me.Attributes['SliceType'] := strSlice[FSliceType];

  me.Attributes['Left'] := IntToStr(FRect.Left);
  me.Attributes['Top'] := IntToStr(FRect.Top);
  me.Attributes['Right'] := IntToStr(FRect.Right);
  me.Attributes['Bottom'] := IntToStr(FRect.Bottom);

  if (IsSingleColor) then
    me.Attributes['SingleColor'] := IntToStr(SingleColor);

  if (IsBackground) then
    me.Attributes['IsBackground'] := 'true';


  for i := 0 to FAttributes.Count - 1 do
  begin
    strName := FAttributes.Names[i];
    strValue := FAttributes.Values[strName];
    if (strName <> '') and (strValue <> '') then
    begin
      attrNode := me.AddChild;
      attrNode.Name := 'attr';
      attrNode.Attributes['name'] := strName;
      attrNode.Attributes['value'] := strValue;
    end;
  end;

  for i := 0 to FKids.Count - 1 do
    FKids[i].SaveToXML(me);

end;

procedure TRectGrid.GroupKids(FromIndex, ToIndex: Integer);
var
  rt: TRect;
  newParent, rg: TRectGrid;
  i: Integer;
begin
  rt.Left := FKids[FromIndex].FRect.Left;
  rt.Top := FKids[FromIndex].FRect.Top;
  rt.Right := FKids[ToIndex].FRect.Right;
  rt.Bottom := FKids[ToIndex].FRect.Bottom;

  newParent := TRectGrid.Create(rt);
  newParent.FSliceType := Self.FSliceType;
  newParent.FParent := Self;

  for i := FromIndex to ToIndex do
  begin
    rg := FKids[FromIndex]; { because we extract FromIndex }
    FKids.FList.Extract(rg);
    newParent.FKids.Add(rg);
    rg.FParent := newParent;
  end;

  FKids.Insert(FromIndex, newParent);

end;

end.
