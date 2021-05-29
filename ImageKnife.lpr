program ImageKnife;

{$MODE Delphi}

uses
  Forms,
  Interfaces,
  MainForm in 'MainForm.pas' {Form1},
  RectGrid in 'RectGrid.pas',
  ImageKnifeDocument in 'ImageKnifeDocument.pas',
  FileUtils in 'FileUtils.pas',
  xml in 'xml.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
