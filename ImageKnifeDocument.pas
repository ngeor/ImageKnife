unit ImageKnifeDocument;

{$MODE Delphi}

interface

uses RectGrid, Classes, Graphics, SysUtils;

type
  TImageKnifeDocument = class
  private
    FNormalImageFilename: string;
    FOverImageFilename: string;
    FRectGrid: TRectGrid;
    FFilename: string;
    FNormalImage: TPicture;
    FOverImage: TPicture;
    function IsEmpty: boolean;
    procedure SetNormalImageFilename(const str: string);
    procedure SetOverImageFilename(const str: string);
    function GetWidth: integer;
    function GetHeight: integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure NewDocument;

    procedure LoadFromFile(const Filename: string);
    procedure SaveToFile(const Filename: string);

    property Empty: boolean read IsEmpty;
    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
    property NormalImage: TPicture read FNormalImage;
    property OverImage: TPicture read FOverImage;
  published
    property NormalImageFilename: string read FNormalImageFilename
      write SetNormalImageFilename;
    property OverImageFilename: string read FOverImageFilename
      write SetOverImageFilename;

    property Grid: TRectGrid read FRectGrid write FRectGrid;

    property Filename: string read FFilename write FFilename;




  end;



implementation

uses FileUtils, xml;

constructor TImageKnifeDocument.Create;
begin
  FNormalImageFilename := '';
  FOverImageFilename := '';
  FFilename := '';
  FRectGrid := nil;
  FNormalImage := TPicture.Create;
  FOverImage := TPicture.Create;
end;

destructor TImageKnifeDocument.Destroy;
begin
  FNormalImage.Free;
  FOverImage.Free;
  if FRectGrid <> nil then
    FRectGrid.Free;
  inherited;
end;

procedure TImageKnifeDocument.NewDocument;
begin
  if FRectGrid <> nil then
    FRectGrid.Free;
  FRectGrid := nil;
  FNormalImageFilename := '';
  FOverImageFilename := '';
  FFilename := '';
end;

function TImageKnifeDocument.IsEmpty: boolean;
begin
  Result := FRectGrid = nil;
end;

function TImageKnifeDocument.GetWidth: integer;
begin
  if IsEmpty then
    Result := 0
  else
    Result := FRectGrid.Rect.Right;
end;

function TImageKnifeDocument.GetHeight: integer;
begin
  if IsEmpty then
    Result := 0
  else
    Result := FRectGrid.Rect.Bottom;
end;

procedure TImageKnifeDocument.SetNormalImageFilename(const str: string);
var
  p: TPicture;
begin
  p := TPicture.Create;
  try
    p.LoadFromFile(str);

    if (IsEmpty) then
    begin
      FNormalImage.Assign(p);
      FNormalImageFileName := str;
      FRectGrid := TRectGrid.Create(Rect(0, 0, p.Width, p.Height));
    end
    else
    begin
      if (p.Width <> Self.Width) or (p.Height <> Self.Height) then
        raise Exception.Create('New normal image is not the same size as the current');
      FNormalImage.Assign(p);
      FNormalImageFileName := str;
    end;

  finally
    p.Free;
  end;
end;

procedure TImageKnifeDocument.SetOverImageFilename(const str: string);
var
  p: TPicture;
begin
  p := TPicture.Create;
  try
    p.LoadFromFile(str);

    if (IsEmpty) then
    begin
      FOverImage.Assign(p);
      FOverImageFileName := str;
      FRectGrid := TRectGrid.Create(Rect(0, 0, p.Width, p.Height));
    end
    else
    begin
      if (p.Width <> Self.Width) or (p.Height <> Self.Height) then
        raise Exception.Create('New normal image is not the same size as the current');
      FOverImage.Assign(p);
      FOverImageFileName := str;
    end;

  finally
    p.Free;
  end;
end;

procedure TImageKnifeDocument.LoadFromFile(const Filename: string);
var
  f: TFileStream;
  r: TXMLReader;
  root: TXMLNode;
  enum: TXMLNodeEnum;
begin
  f := TFileStream.Create(Filename, fmOpenRead);
  r := TXMLReader.Create(f);

  root := r.Read;

  r.Free;
  f.Free;

  if root.Name <> 'root' then
    raise Exception.Create('root tag expected');

  FNormalImageFilename := root.Attributes['NormalImageFilename'];
  FOverImageFilename := root.Attributes['OverImageFilename'];

  enum := root.GetNodesByName('cell');
  if enum.Count <> 1 then
    raise Exception.Create('Only one master cell is allowed');

  if FRectGrid <> nil then
    FRectGrid.Free;
  FRectGrid := TRectGrid.Create(Rect(0, 0, 0, 0));
  FRectGrid.LoadFromXML(enum[0]);

  enum.Free;

  root.Free;



  NormalImage.LoadFromFile(NormalImageFilename);

  if OverImageFilename <> '' then
  begin
    OverImage.LoadFromFile(OverImageFilename);
    if (NormalImage.Width <> OverImage.Width) or
      (NormalImage.Height <> OverImage.Height) then
      raise Exception.Create('Image dimensions must be the same');
    if (Grid.Rect.Right <> NormalImage.Width) or (Grid.Rect.Bottom <>
      NormalImage.Height) then
      raise Exception.Create('Grid dimensions dont match images');
    if (Grid.Rect.Left <> 0) or (Grid.Rect.Top <> 0) then
      raise Exception.Create('Master rect must begin at top left');
  end;

end;

procedure TImageKnifeDocument.SaveToFile(const Filename: string);
var
  f: TFileStream;
  w: TXMLWriter;
  root: TXMLNode;
begin
  if Empty then
    Exit;
  f := TFileStream.Create(Filename, fmCreate);
  w := TXMLWriter.Create(f);

  root := TXMLNode.Create;

  root.Name := 'root';
  root.Attributes['NormalImageFilename'] := FNormalImageFilename;
  root.Attributes['OverImageFilename'] := FOverImageFilename;


  FRectGrid.SaveToXML(root);

  w.Write(root);

  root.Free;
  w.Free;
  f.Free;
end;

end.
