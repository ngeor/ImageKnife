unit MainForm;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs,
  ExtCtrls, ComCtrls, Buttons, Contnrs, ExtDlgs, StdCtrls,
  Menus,
  ImageKnifeDocument, RectGrid, Grids;

type

  { TForm1 }

  TForm1 = class(TForm)
    StatusBar1: TStatusBar;
    Panel1: TPanel;
    OpenPictureDialog1: TOpenPictureDialog;
    btnExport: TSpeedButton;
    SaveDialog1: TSaveDialog;
    btnAutoName: TButton;
    tbCompression: TTrackBar;
    MainMenu1: TMainMenu;
    mnuFile: TMenuItem;
    mnuFileNew: TMenuItem;
    mnuFileOpen: TMenuItem;
    mnuFileSave: TMenuItem;
    mnuFileSaveAs: TMenuItem;
    N1: TMenuItem;
    mnuFileExport: TMenuItem;
    N2: TMenuItem;
    mnuFileExit: TMenuItem;
    mnuFileClose: TMenuItem;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    saveKnife: TSaveDialog;
    openKnife: TOpenDialog;
    popSlice: TPopupMenu;
    Deleteslice1: TMenuItem;
    mnuView: TMenuItem;
    mnuViewRefresh: TMenuItem;
    Panel2: TPanel;
    TreeView1: TTreeView;
    Memo1: TMemo;
    ScrollBox1: TScrollBox;
    Splitter1: TSplitter;
    PaintBox1: TPaintBox;
    N3: TMenuItem;
    mnuViewZoomIn: TMenuItem;
    mnuViewZoomOut: TMenuItem;
    mnuViewZoomNormal: TMenuItem;
    Splitter2: TSplitter;
    Panel3: TPanel;
    chkSingleColor: TCheckBox;
    Label1: TLabel;
    lblSingleColor: TLabel;
    btnPickSingleColor: TSpeedButton;
    btnScan1: TButton;
    Panel4: TPanel;
    btnSelector: TSpeedButton;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    chkAcross: TCheckBox;
    btnSelectParents: TSpeedButton;
    btnGroup: TSpeedButton;
    chkBackground: TCheckBox;
    Label3: TLabel;
    cmbVAlign: TComboBox;
    btnRefreshWidth: TBitBtn;
    StringGrid1: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure mnuFileExitClick(Sender: TObject);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure TreeView1Edited(Sender: TObject; Node: TTreeNode; var S: string);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure btnAutoNameClick(Sender: TObject);
    procedure mnuFileNewClick(Sender: TObject);
    procedure mnuFileSaveClick(Sender: TObject);
    procedure mnuFileSaveAsClick(Sender: TObject);
    procedure mnuFileOpenClick(Sender: TObject);
    procedure mnuViewRefreshClick(Sender: TObject);
    procedure mnuFileExportClick(Sender: TObject);
    procedure mnuViewZoomNormalClick(Sender: TObject);
    procedure mnuViewZoomInClick(Sender: TObject);
    procedure mnuViewZoomOutClick(Sender: TObject);
    procedure chkSingleColorClick(Sender: TObject);
    procedure btnScan1Click(Sender: TObject);
    procedure btnSelectorClick(Sender: TObject);
    procedure btnSelectParentsClick(Sender: TObject);
    procedure btnGroupClick(Sender: TObject);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure chkBackgroundClick(Sender: TObject);
    procedure cmbVAlignChange(Sender: TObject);
    procedure btnRefreshWidthClick(Sender: TObject);
    procedure StringGrid1SetEditText(Sender: TObject; ACol, ARow: integer;
      const Value: string);
  private
    document: TImageKnifeDocument;

    rgSelection: TObjectList;
    OldMarker: TPoint;
    MarkerMode: (mmSelect, mmSelectParents, mmHorizontal, mmVertical, mmCross);

    zoomLevel: double;

    procedure DrawCross(const ptPhysical: TPoint);
    procedure DrawHorizontal(const ptPhysical: TPoint);
    procedure DrawVertical(const ptPhysical: TPoint);
    procedure DrawMarker(const ptLogical: TPoint);
    procedure SaveRect(const Filename: string; const rt: TRect);
    procedure BuildTree;
    procedure BuildHTML;
    procedure BuildTreeOneLevel(const node: TTreeNode; const rg: TRectGrid);

    procedure ResizePaintBox;
    procedure DocumentChanged;

    procedure SelectionChanged;

    function PhysicalPoint(const ptLogical: TPoint): TPoint;
    function LogicalPoint(const ptPhysical: TPoint): TPoint;

    function PhysicalRect(const rtLogical: TRect): TRect;
    function LogicalRect(const rtPhysical: TRect): TRect;

    procedure InitializePropertyGrid;

    function GetGridProperty(const Index: string): string;
    procedure SetGridProperty(const Index: string; const Value: string);
  public
    property GridProperty[const Index: string]: string
      read GetGridProperty write SetGridProperty;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.DrawCross(const ptPhysical: TPoint);
begin
  with PaintBox1.Canvas do
  begin

    MoveTo(0, ptPhysical.Y);
    LineTo(ptPhysical.X, ptPhysical.Y);
    LineTo(ptPhysical.X, 0);
    MoveTo(PaintBox1.Width, ptPhysical.Y);
    LineTo(ptPhysical.X, ptPhysical.Y);
    LineTo(ptPhysical.X, PaintBox1.Height);
  end;
end;

procedure TForm1.DrawHorizontal(const ptPhysical: TPoint);
begin
  with PaintBox1.Canvas do
  begin
    MoveTo(0, ptPhysical.Y);
    LineTo(PaintBox1.Width, ptPhysical.Y);
  end;
end;

procedure TForm1.DrawVertical(const ptPhysical: TPoint);
begin
  with PaintBox1.Canvas do
  begin
    MoveTo(ptPhysical.X, 0);
    LineTo(ptPhysical.X, PaintBox1.Height);
  end;
end;

function TForm1.PhysicalPoint(const ptLogical: TPoint): TPoint;
begin
  Result.X := Round(ptLogical.X * zoomLevel);
  Result.Y := Round(ptLogical.Y * zoomLevel);
end;

function TForm1.LogicalPoint(const ptPhysical: TPoint): TPoint;
begin
  Result.X := Round(ptPhysical.X / zoomLevel);
  Result.Y := Round(ptPhysical.Y / zoomLevel);
end;

function TForm1.PhysicalRect(const rtLogical: TRect): TRect;
begin
  Result.Left := Round(rtLogical.Left * zoomLevel);
  Result.Top := Round(rtLogical.Top * zoomLevel);
  Result.Right := Round(rtLogical.Right * zoomLevel);
  Result.Bottom := Round(rtLogical.Bottom * zoomLevel);
end;

function TForm1.LogicalRect(const rtPhysical: TRect): TRect;
begin
  Result.Left := Round(rtPhysical.Left / zoomLevel);
  Result.Top := Round(rtPhysical.Top / zoomLevel);
  Result.Right := Round(rtPhysical.Right / zoomLevel);
  Result.Bottom := Round(rtPhysical.Bottom / zoomLevel);
end;

procedure TForm1.DrawMarker(const ptLogical: TPoint);
var
  ptPhysical: TPoint;
begin
  if document.Empty then
    Exit;
  if (0 = ptLogical.X) and (0 = ptLogical.Y) then
    exit;
  ptPhysical := PhysicalPoint(ptLogical);
  PaintBox1.Canvas.Pen.Mode := pmNot;
  case MarkerMode of
    mmHorizontal: DrawHorizontal(ptPhysical);
    mmVertical: DrawVertical(ptPhysical);
    mmCross: DrawCross(ptPhysical);
  end;
  PaintBox1.Canvas.Pen.Mode := pmCopy;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  rgSelection := TObjectList.Create(False);

  document := TImageKnifeDocument.Create;

  zoomLevel := 1;

  OldMarker.X := 0;
  OldMarker.Y := 0;
  MarkerMode := mmSelect;

  BuildTree;
  InitializePropertyGrid;
end;

procedure TForm1.mnuFileExitClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.SaveRect(const Filename: string; const rt: TRect);
var
  bmp: TBitmap;
  //png: TPNGObject;
begin

  bmp := TBitmap.Create;
  bmp.Width := rt.Right - rt.Left;
  bmp.Height := rt.Bottom - rt.Top;
  bmp.Canvas.CopyRect(Rect(0, 0, bmp.Width, bmp.Height),
    document.NormalImage.Bitmap.Canvas, rt);

  //png := TPNGObject.Create;
  //png.Assign(bmp);

  bmp.Free;

  //png.CompressionLevel := tbCompression.Position;


  //png.SaveToFile(Filename);
  //png.Free;
end;

procedure TForm1.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
var
  pt: TPoint;
begin
  DrawMarker(OldMarker);
  pt := LogicalPoint(Point(X, Y));

  DrawMarker(pt);
  OldMarker := pt;
  StatusBar1.SimpleText := IntToStr(OldMarker.X) + '-' + IntToStr(OldMarker.Y);
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  DrawMarker(OldMarker);
  OldMarker.X := 0;
  OldMarker.Y := 0;
  MarkerMode := mmHorizontal;
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
begin
  DrawMarker(OldMarker);
  OldMarker.X := 0;
  OldMarker.Y := 0;
  MarkerMode := mmVertical;
end;

procedure TForm1.SpeedButton3Click(Sender: TObject);
begin
  DrawMarker(OldMarker);
  OldMarker.X := 0;
  OldMarker.Y := 0;
  MarkerMode := mmCross;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  document.Free;
  rgSelection.Free;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
const
  stColor: array [TSliceType] of TColor = (clGray, clRed, clBlue);
var
  k: TObjectList;
  rg: TRectGrid;
  x, y: integer;
  i: integer;
  physRect: TRect;
begin
  if document.Empty then
    Exit;
  k := TObjectList.Create(False);
  k.Add(document.Grid);

  PaintBox1.Canvas.StretchDraw(PaintBox1.ClientRect, document.NormalImage.Bitmap);
  //PaintBox1.Canvas.CopyRect(PaintBox1.ClientRect, document.NormalImage.Bitmap.Canvas, PaintBox1.ClientRect);
  PaintBox1.Canvas.Brush.Color := clBlue;
  PaintBox1.Canvas.Brush.Style := bsSolid;
  while k.Count > 0 do
  begin
    rg := k[0] as TRectGrid;
    physRect := PhysicalRect(rg.Rect);
    k.Delete(0);
    if rg.Sliced then
    begin
      for i := 0 to rg.Kids.Count - 1 do
        k.Add(rg.Kids[i]);
    end
    else
    begin
      PaintBox1.Canvas.FrameRect(physRect);
    end;
  end;

  for i := 0 to rgSelection.Count - 1 do
  begin
    rg := rgSelection[i] as TRectGrid;
    physRect := PhysicalRect(rg.Rect);
    for y := physRect.Top to physRect.Bottom - 1 do
      for x := physRect.Left to physRect.Right - 1 do
        if ((x mod 3) = 0) and ((y mod 3) = 0) then
          PaintBox1.Canvas.Pixels[x, y] := stColor[rg.SliceType];
  end;

  k.Free;
end;

procedure TForm1.BuildTree;
begin
  TreeView1.Items.Clear;
  if not document.Empty then
  begin
    BuildTreeOneLevel(nil, document.Grid);
    TreeView1.Items[0].Expand(True);
  end;
  BuildHTML;
end;

procedure TForm1.BuildTreeOneLevel(const node: TTreeNode; const rg: TRectGrid);
var
  nn: TTreeNode;
  i: integer;
begin
  nn := TreeView1.Items.AddChild(node, rg.Name);
  nn.Data := rg;
  if (rg.Sliced) then
    for i := 0 to rg.Kids.Count - 1 do
      BuildTreeOneLevel(nn, rg.Kids[i]);
end;

procedure TForm1.TreeView1Edited(Sender: TObject; Node: TTreeNode; var S: string);
var
  rg: TRectGrid;
begin
  rg := TRectGrid(Node.Data);
  rg.Name := S;

end;

procedure TForm1.TreeView1Change(Sender: TObject; Node: TTreeNode);
var
  rg: TRectGrid;
begin
  DrawMarker(OldMarker);

  rg := TRectGrid(Node.Data);

  rgSelection.Clear;
  rgSelection.Add(rg);

  SelectionChanged;
  PaintBox1.Refresh;

  DrawMarker(OldMarker);
end;

procedure TForm1.btnAutoNameClick(Sender: TObject);

  procedure RenameKids(rg: TRectGrid);
  var
    i: integer;
    s: string;
  begin
    if not rg.Sliced then
      Exit;
    if rg.SliceType = stHorizontal then
      s := 'r'
    else
      s := 'c';

    for i := 0 to rg.Kids.Count - 1 do
    begin
      rg.Kids[i].Name := rg.Name + '-' + s + IntToStr(i + 1);
      RenameKids(rg.Kids[i]);
    end;

  end;

begin
  if document.Empty then
    Exit;
  document.Grid.Name := 'cell';
  RenameKids(document.Grid);
  BuildTree;
end;

function HTMLColor(col: DWORD): string;
begin
  Result := '#' + IntToHex(GetRValue(col), 2) + IntToHex(GetGValue(col), 2) +
    IntToHex(GetBValue(col), 2);
end;

procedure TForm1.BuildHTML;

  procedure AddText(const template, Value: string);
  begin
    if Value <> '' then
      Memo1.Text := Memo1.Text + StringReplace(template, '?', Value, []);
  end;

  procedure OpenTD(const rg: TRectGrid);
  begin
    Memo1.Lines.Append('<td');
    AddText(' valign="?"', rg.Attributes['valign']);
    if rg.IsSingleColor then
      AddText(' bgcolor="?"', HTMLColor(rg.SingleColor));

    if rg.IsBackground then
    begin
      Memo1.Text := Memo1.Text + ' style="';
      AddText('background-repeat: ?;', rg.Attributes['background-repeat']);
      AddText('background-position: ?;', rg.Attributes['background-position']);
      AddText('background-image:url(''?'')', rg.Name + '.png');
      Memo1.Text := Memo1.Text + '"';
    end;
    AddText(' width="?"', rg.Attributes['width']);
    Memo1.Text := Memo1.Text + '>';
  end;

  procedure WriteSimpleCell(const rg: TRectGrid);
  begin
    OpenTD(rg);
    AddText('<a href="?">', rg.Attributes['href']);

    if rg.IsSingleColor or rg.IsBackground then
      Memo1.Text := Memo1.Text + rg.Attributes['text']
    else
      Memo1.Text := Memo1.Text + '<img alt="" border="0" src="' + rg.Name + '.png">';

    AddText('</a>', rg.Attributes['href']);

    Memo1.Text := Memo1.Text + '</td>';
  end;

  procedure WriteHTMLTable(const rg: TRectGrid);
  var
    i, j: integer;
  begin
    for i := 0 to rg.Kids.Count - 1 do
    begin
      Memo1.Lines.Append('<tr>');
      for j := 0 to rg.Kids[i].Kids.Count - 1 do
        WriteSimpleCell(rg.Kids[i].Kids[j]);
      Memo1.Lines.Append('</tr>');
    end;
  end;

  procedure WriteInvertedHTMLTable(const rg: TRectGrid);
  var
    i, j: integer;
  begin
    for j := 0 to rg.Kids[0].Kids.Count - 1 do
    begin
      Memo1.Lines.Append('<tr>');
      for i := 0 to rg.Kids.Count - 1 do
        WriteSimpleCell(rg.Kids[i].Kids[j]);
      Memo1.Lines.Append('</tr>');
    end;
  end;

  function IsHTMLTable(const rg: TRectGrid): boolean;
  var
    i, j: integer;
    oldcount: integer;
    oldwidth: integer;
  begin
    Result := False;
    if (rg.SliceType = stHorizontal) then
    begin
      for i := 0 to rg.Kids.Count - 1 do
        if rg.Kids[i].SliceType <> stVertical then
          Exit;
      oldcount := rg.Kids[0].Kids.Count;
      for i := 1 to rg.Kids.Count - 1 do
        if oldcount <> rg.Kids[i].Kids.Count then
          Exit;
      for i := 0 to rg.Kids.Count - 1 do
        for j := 0 to rg.Kids[i].Kids.Count - 1 do
          if rg.Kids[i].Kids[j].Sliced then
            Exit;
      for j := 0 to oldcount - 1 do
      begin
        oldwidth := rg.Kids[0].Kids[j].Rect.Right - rg.Kids[0].Kids[j].Rect.Left;
        for i := 1 to rg.Kids.Count - 1 do
          if oldwidth <> (rg.Kids[i].Kids[j].Rect.Right -
            rg.Kids[i].Kids[j].Rect.Left) then
            Exit;
      end;
    end;
    Result := True;
  end;

  function IsInvertedHTMLTable(const rg: TRectGrid): boolean;
  var
    i, j: integer;
    oldcount: integer;
    oldheight: integer;
  begin
    Result := False;
    if (rg.SliceType = stVertical) then
    begin
      for i := 0 to rg.Kids.Count - 1 do
        if rg.Kids[i].SliceType <> stHorizontal then
          Exit;
      oldcount := rg.Kids[0].Kids.Count;
      for i := 1 to rg.Kids.Count - 1 do
        if oldcount <> rg.Kids[i].Kids.Count then
          Exit;
      for i := 0 to rg.Kids.Count - 1 do
        for j := 0 to rg.Kids[i].Kids.Count - 1 do
          if rg.Kids[i].Kids[j].Sliced then
            Exit;
      for j := 0 to oldcount - 1 do
      begin
        oldheight := rg.Kids[0].Kids[j].Rect.Bottom - rg.Kids[0].Kids[j].Rect.Top;
        for i := 1 to rg.Kids.Count - 1 do
          if oldheight <> (rg.Kids[i].Kids[j].Rect.Bottom -
            rg.Kids[i].Kids[j].Rect.Top) then
            Exit;
      end;
    end;
    Result := True;
  end;

  procedure BuildHTMLOneLevel(const rg: TRectGrid);
  var
    i: integer;
  begin
    if rg.SliceType = stNone then
      Memo1.Text := Memo1.Text + '<img src="' + rg.Name + '.png">'
    else
    begin
      Memo1.Lines.Append('<table');
      AddText(' width="?"', rg.Attributes['width']);
      Memo1.Text := Memo1.Text + ' border="0" cellpadding="0" cellspacing="0">';
      if rg.SliceType = stHorizontal then
      begin
        if (IsHTMLTable(rg)) then
          WriteHTMLTable(rg)
        else
          for i := 0 to rg.Kids.Count - 1 do
          begin
            if (rg.Kids[i].Sliced) then
            begin
              Memo1.Lines.Append('<tr>');
              OpenTD(rg.Kids[i]);
              BuildHTMLOneLevel(rg.Kids[i]);
              Memo1.Text := Memo1.Text + '</td>';
              Memo1.Lines.Append('</tr>');
            end
            else
            begin
              Memo1.Lines.Append('<tr>');
              WriteSimpleCell(rg.Kids[i]);
              Memo1.Lines.Append('</tr>');
            end;
          end;

      end
      else
      begin { stVertical }
        if (IsInvertedHTMLTable(rg)) then
          WriteInvertedHTMLTable(rg)
        else
        begin
          Memo1.Lines.Append('<tr>');
          for i := 0 to rg.Kids.Count - 1 do
          begin
            if (rg.Kids[i].Sliced) then
            begin
              OpenTD(rg.Kids[i]);
              BuildHTMLOneLevel(rg.Kids[i]);
              Memo1.Text := Memo1.Text + '</td>';
            end
            else
            begin
              WriteSimpleCell(rg.Kids[i]);
            end;
          end;
          Memo1.Lines.Append('</tr>');
        end;
      end;
      Memo1.Lines.Append('</table>');
    end;
  end;

begin
  Memo1.Text := '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"';
  Memo1.Lines.Append('"http://www.w3.org/TR/html4/loose.dtd">');
  Memo1.Lines.Append('<html>');
  Memo1.Lines.Append('<head>');
  Memo1.Lines.Append('<title>QuickWeb</title>');
  Memo1.Lines.Append('<meta http-equiv="Content-Type" content="text/html;charset=utf-8" >');
  Memo1.Lines.Append('</head>');
  Memo1.Lines.Append('<body>');
  if not document.Empty then
    BuildHTMLOneLevel(document.Grid);
  Memo1.Lines.Append('</body></html>');
end;

procedure TForm1.mnuFileNewClick(Sender: TObject);
//var
//tmp: TImageKnifeDocument;
begin
  //tmp := TImageKnifeDocument.Create;
  rgSelection.Clear;
  try
    OpenPictureDialog1.Title := 'Select normal image';
    if OpenPictureDialog1.Execute then
    begin
      document.NormalImageFilename := OpenPictureDialog1.Filename;

      OpenPictureDialog1.Title := 'Select rollover image';
      if OpenPictureDialog1.Execute then
      begin
        document.OverImageFilename := OpenPictureDialog1.Filename;
      end;

      //document.Free;
      //document := tmp;
      //tmp := nil;

      DocumentChanged;
    end;
  finally
    //if tmp <> nil then tmp.Free;
  end;
end;

procedure TForm1.mnuFileSaveClick(Sender: TObject);
begin
  if document.Filename = '' then
    mnuFileSaveAsClick(Sender)
  else
    document.SaveToFile(document.Filename);
end;

procedure TForm1.mnuFileSaveAsClick(Sender: TObject);
begin
  if saveKnife.Execute then
  begin
    document.SaveToFile(saveKnife.Filename);
    document.Filename := saveKnife.Filename;
  end;
end;

procedure TForm1.mnuFileOpenClick(Sender: TObject);
//var
//tmp: TImageKnifeDocument;
begin
  //tmp := TImageKnifeDocument.Create;
  rgSelection.Clear;
  try

    if openKnife.Execute then
    begin

      document.LoadFromFile(openKnife.Filename);
      document.Filename := openKnife.Filename;

      //document.Free;
      //document := tmp;
      //tmp := nil;

      DocumentChanged;
    end;
  finally
    //if tmp <> nil then tmp.Free;
  end;
end;

procedure TForm1.mnuViewRefreshClick(Sender: TObject);
begin
  PaintBox1.Refresh;
  DrawMarker(OldMarker);
end;

procedure TForm1.mnuFileExportClick(Sender: TObject);
var
  strPath: string;

  k: TObjectList;
  rg: TRectGrid;

  i: integer;

begin
  if document.Empty then
    Exit;
  with SaveDialog1 do
    if Execute then
    begin
      strPath := ExtractFilePath(Filename);
      k := TObjectList.Create(False);
      k.Add(document.Grid);

      while k.Count > 0 do
      begin
        rg := k[0] as TRectGrid;
        k.Delete(0);
        if rg.Sliced then
        begin
          for i := 0 to rg.Kids.Count - 1 do
            k.Add(rg.Kids[i]);
        end
        else
        begin
          if not rg.IsSingleColor then
            SaveRect(strPath + rg.Name + '.png', rg.Rect);
        end;
      end;

      k.Free;

      Memo1.Lines.SaveToFile(FileName);

    end;

end;

procedure TForm1.ResizePaintBox;
begin
  PaintBox1.SetBounds(PaintBox1.Left, PaintBox1.Top, Round(zoomLevel * document.Width),
    Round(zoomLevel * document.Height));
end;

procedure TForm1.DocumentChanged;
begin
  ResizePaintBox;
  //PaintBox1.Repaint;
  BuildTree;
end;

procedure TForm1.mnuViewZoomNormalClick(Sender: TObject);
begin
  zoomLevel := 1;
  ResizePaintBox;
end;

procedure TForm1.mnuViewZoomInClick(Sender: TObject);
begin
  zoomLevel := zoomLevel * 2;
  ResizePaintBox;
end;

procedure TForm1.mnuViewZoomOutClick(Sender: TObject);
begin
  zoomLevel := zoomLevel / 2;
  ResizePaintBox;
end;

procedure TForm1.chkSingleColorClick(Sender: TObject);
var
  rg: TRectGrid;
begin
  if rgSelection.Count = 1 then
  begin
    rg := rgSelection[0] as TRectGrid;
    rg.IsSingleColor := chkSingleColor.Checked;
    if rg.IsSingleColor then
    begin
      rg.SingleColor := document.NormalImage.Bitmap.Canvas.Pixels[
        rg.Rect.Left, rg.Rect.Top];
      lblSingleColor.Color := rg.SingleColor;
    end;
  end;
end;

procedure TForm1.btnScan1Click(Sender: TObject);
var
  k: TObjectList;
  rg: TRectGrid;
  i: integer;

  x, y: integer;

  lastColor: TColor;

  rt: TRect;

  found: boolean;
begin
  if document.Empty then
    Exit;
  k := TObjectList.Create(False);
  k.Add(document.Grid);

  while k.Count > 0 do
  begin
    rg := k[0] as TRectGrid;
    k.Delete(0);
    if rg.Sliced then
    begin
      for i := 0 to rg.Kids.Count - 1 do
        k.Add(rg.Kids[i]);
    end
    else
    begin
      rt := rg.Rect;
      found := False;
      lastColor := document.NormalImage.Bitmap.Canvas.Pixels[rt.Left, rt.Top];
      for y := rt.Top to rt.Bottom - 1 do
        for x := rt.Left to rt.Right - 1 do
          if lastColor <> document.NormalImage.Bitmap.Canvas.Pixels[x, y] then
          begin
            found := True;
            Break;
          end;
      if not found then
      begin
        rg.IsSingleColor := True;
        rg.SingleColor := lastColor;
      end;
    end;
  end;

  k.Free;

end;

procedure TForm1.btnSelectorClick(Sender: TObject);
begin
  rgSelection.Clear;
  PaintBox1.Repaint;
  DrawMarker(OldMarker);
  OldMarker.X := 0;
  OldMarker.Y := 0;
  MarkerMode := mmSelect;
end;

procedure TForm1.btnSelectParentsClick(Sender: TObject);
begin
  rgSelection.Clear;
  PaintBox1.Repaint;
  DrawMarker(OldMarker);
  OldMarker.X := 0;
  OldMarker.Y := 0;
  MarkerMode := mmSelectParents;
end;

procedure TForm1.btnGroupClick(Sender: TObject);
var
  iMin, iMax: integer;
  i, iCur: integer;
  rg, oldParent: TRectGrid;
begin
  if rgSelection.Count <= 1 then
    Exit;
  rg := rgSelection[0] as TRectGrid;
  oldParent := rg.Parent;
  iMin := oldParent.Kids.IndexOf(rg);
  iMax := iMin;

  for i := 1 to rgSelection.Count - 1 do
  begin
    rg := rgSelection[i] as TRectGrid;
    if rg.Parent <> oldParent then
      raise Exception.Create('All selected rects must belong to the same parent');
    iCur := oldParent.Kids.IndexOf(rg);
    if iCur < iMin then
      iMin := iCur;
    if iCur > iMax then
      iMax := iCur;
  end;

  if (iMax - iMin + 1 > rgSelection.Count) then
    raise Exception.Create('Selection must be contiguous');
  rgSelection.Clear;
  oldParent.GroupKids(iMin, iMax);
  PaintBox1.Repaint;
  BuildTree;
end;

procedure TForm1.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);

var
  k: TObjectList;
  rg: TRectGrid;
  i: integer;

begin
  if document.Empty then
    Exit;

  DrawMarker(OldMarker);

  k := TObjectList.Create(False);

  if (chkAcross.Checked) and (MarkerMode in [mmHorizontal, mmVertical]) then
  begin
    if MarkerMode = mmHorizontal then
      document.Grid.RectsFromY(OldMarker.Y, k)
    else if MarkerMode = mmVertical then
      document.Grid.RectsFromX(OldMarker.X, k);
  end
  else
    k.Add(document.Grid.RectFromPos(Point(OldMarker.X, OldMarker.Y)));

  if MarkerMode = mmSelect then
  begin
    if ssCtrl in Shift then
    begin
      for i := 0 to k.Count - 1 do
      begin
        rg := k[i] as TRectGrid;
        if (rgSelection.IndexOf(rg) < 0) then
          rgSelection.Add(rg)
        else
          rgSelection.Remove(rg);
      end;
    end
    else
    begin
      rgSelection.Clear;
      for i := 0 to k.Count - 1 do
      begin
        rg := k[i] as TRectGrid;
        rgSelection.Add(rg);
      end;
    end;
  end
  else if MarkerMode = mmSelectParents then
  begin
    if ssCtrl in Shift then
    begin
      for i := 0 to k.Count - 1 do
      begin
        rg := (k[i] as TRectGrid).Parent;
        if (rgSelection.IndexOf(rg) < 0) then
          rgSelection.Add(rg)
        else
          rgSelection.Remove(rg);
      end;
    end
    else
    begin
      rgSelection.Clear;
      for i := 0 to k.Count - 1 do
      begin
        rg := (k[i] as TRectGrid).Parent;
        rgSelection.Add(rg);
      end;
    end;
  end
  else
  begin
    for i := 0 to k.Count - 1 do
    begin
      rg := k[i] as TRectGrid;
      case MarkerMode of
        mmHorizontal: rg.SliceHorizontal(OldMarker.Y);
        mmVertical: rg.SliceVertical(OldMarker.X);
      end;
    end;
  end;

  PaintBox1.Refresh;
  DrawMarker(OldMarker);

  k.Free;

  if MarkerMode in [mmHorizontal, mmVertical] then
    BuildTree
  else if MarkerMode in [mmSelect, mmSelectParents] then
    SelectionChanged;
end;

procedure TForm1.InitializePropertyGrid;
begin
  StringGrid1.RowCount := 5;
  StringGrid1.ColCount := 2;


  StringGrid1.Cells[0, 0] := 'text';
  StringGrid1.Cells[0, 1] := 'href';
  StringGrid1.Cells[0, 2] := 'width';
  StringGrid1.Cells[0, 3] := 'background-repeat';
  StringGrid1.Cells[0, 4] := 'background-position';
end;

function TForm1.GetGridProperty(const Index: string): string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to StringGrid1.RowCount - 1 do
    if StringGrid1.Cells[0, i] = Index then
    begin
      Result := StringGrid1.Cells[1, i];
      Break;
    end;
end;

procedure TForm1.SetGridProperty(const Index: string; const Value: string);
var
  i: integer;
begin

  for i := 0 to StringGrid1.RowCount - 1 do
    if StringGrid1.Cells[0, i] = Index then
    begin
      StringGrid1.Cells[1, i] := Value;
      Break;
    end;
end;

procedure TForm1.SelectionChanged;
var
  rg: TRectGrid;
  i: integer;
begin
  if rgSelection.Count = 1 then
  begin
    rg := rgSelection[0] as TRectGrid;
    chkSingleColor.Checked := rg.IsSingleColor;
    if rg.IsSingleColor then
    begin
      lblSingleColor.Color := rg.SingleColor;
      //lblSingleColor.Visible := True;
      //btnPickSingleColor.Visible := True;
    end
    else
    begin
      //lblSingleColor.Enabled := False;
      //btnPickSingleColor.Enabled := False;
    end;
    chkBackground.Checked := rg.IsBackground;
    cmbVAlign.ItemIndex := cmbVAlign.Items.IndexOf(rg.Attributes['valign']);

    for i := 0 to StringGrid1.RowCount - 1 do
      StringGrid1.Cells[1, i] := rg.Attributes[StringGrid1.Cells[0, i]];
  end;
end;

procedure TForm1.chkBackgroundClick(Sender: TObject);
var
  rg: TRectGrid;
  i: integer;
begin
  for i := 0 to rgSelection.Count - 1 do
  begin
    rg := rgSelection[i] as TRectGrid;
    rg.IsBackground := chkBackground.Checked;
  end;

end;

procedure TForm1.cmbVAlignChange(Sender: TObject);
var
  rg: TRectGrid;
  i: integer;
begin
  for i := 0 to rgSelection.Count - 1 do
  begin
    rg := rgSelection[i] as TRectGrid;
    rg.Attributes['valign'] := cmbVAlign.Text;
  end;

end;

procedure TForm1.btnRefreshWidthClick(Sender: TObject);
var
  rg: TRectGrid;
  i: integer;
begin
  for i := 0 to rgSelection.Count - 1 do
  begin
    rg := rgSelection[i] as TRectGrid;
    rg.Attributes['width'] := IntToStr(rg.Rect.Right - rg.Rect.Left);
    GridProperty['width'] := rg.Attributes['width']; { stupid code }
  end;

end;

procedure TForm1.StringGrid1SetEditText(Sender: TObject; ACol, ARow: integer;
  const Value: string);
var
  rg: TRectGrid;
  i: integer;
  strName: string;
begin
  strName := StringGrid1.Cells[0, ARow];
  for i := 0 to rgSelection.Count - 1 do
  begin
    rg := rgSelection[i] as TRectGrid;
    rg.Attributes[strName] := Value;
  end;

end;

end.
