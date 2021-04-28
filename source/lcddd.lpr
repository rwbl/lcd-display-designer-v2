program lcddd;
(*
  LCD Display Designer
  (c) Robert W.B.Linn - see README.md
*)
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, uMain, uCodeViewer, uLCDTableChars;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='LCD Display Designer';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormCodeViewer, FormCodeViewer);
  Application.CreateForm(TFormLCDTableChars, FormLCDTableChars);
  Application.Run;
end.

