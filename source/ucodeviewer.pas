unit uCodeViewer;
(*
  View the generated code in a syneditor with syntax highligher.
  The code can be modified, copied or saved.
  20210421 rwbl
*)
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ClipBrd, ActnList, Buttons, SynEdit, SynHighlighterCpp, SynHighlighterVB,
  SynHighlighterPython, SynHighlighterBat,
  uMisc;

const
  CODE_VIEWER_TITLE = 'Code Viewer';

type

  { TFormCodeViewer }

  TFormCodeViewer = class(TForm)
    ActionFileSave: TAction;
    ActionCopyCode: TAction;
    ActionList: TActionList;
    PanelTop: TPanel;
    SaveDialogCodeViewer: TSaveDialog;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SynBatSyn: TSynBatSyn;
    SynEditCode: TSynEdit;
    SynCPPSyn: TSynCppSyn;
    SynB4RSyn: TSynVBSyn;
    SynPythonSyn: TSynPythonSyn;
    procedure ActionCopyCodeExecute(Sender: TObject);
    procedure ActionFileSaveExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
    CodeType: String;
  end;

var
  FormCodeViewer: TFormCodeViewer;

implementation

{$R *.lfm}

//
// TFormCodeViewer
//

{ TODO 3 -cCodeViewer : Find out how to add a type to a syntax highligher. Example: CPP, type uint8_t. Something like AddSpecialAttribute('uint8_t','uint8_t'); }
procedure TFormCodeViewer.FormCreate(Sender: TObject);
begin
  SynCPPSyn.AddSpecialAttribute('uint8_t','uint8_t');
end;

procedure TFormCodeViewer.FormShow(Sender: TObject);
begin
  Caption := Format(
    '%s - %s',
    [CODE_VIEWER_TITLE, CodeType]);
end;

//
// ACTIONS
//

procedure TFormCodeViewer.ActionCopyCodeExecute(Sender: TObject);
begin
  Clipboard.AsText := SynEditCode.Lines.Text;
end;

procedure TFormCodeViewer.ActionFileSaveExecute(Sender: TObject);
var
  sl: TStringList;
begin
  if SaveDialogCodeViewer.Execute then begin
    try
      sl := TStringList.Create;
      sl.Text := SynEditCode.Lines.Text;
      sl.SaveToFile(SaveDialogCodeViewer.FileName);
      sl.Free;
    except on E: Exception do
       ShowError(
        'Save Code',
        E.Message + LineEnding + SaveDialogCodeViewer.FileName);
    end;
  end;
end;

end.

