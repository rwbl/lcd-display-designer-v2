unit uMain;
(*
  LCD Display Designer main unit.
*)
{$mode objfpc}{$H+}
// {$H-}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ActnList,
  ExtCtrls, StdCtrls, ComCtrls, Buttons, StrUtils, ClipBrd,
  LCLType, Grids,
  fpJson, jsonParser,
  uMisc,
  uCodeViewer,
  uLCDTableChars;

const
  APPVERSION          = 'v1.0.0 (Build 20210427)';
  APPTITLE            = 'LCD Display Designer';
  APPTITLEVERSION     = APPTITLE + ' ' + APPVERSION;
  APPDESCRIPTION      = APPTITLEVERSION + LineEnding + LineEnding +
    'Open Source Project for creating LCD display layouts connected to Arduino, Raspberry Pi, Tinkerforge or other.' + LineEnding +
    '(c) 2021 by Robert W.B. Linn, Pinneberg, Germany'+LineEnding+LineEnding+
    'Developed with Lazarus v2.0.12, FPC 3.2.0, SVN 64642.'+LineEnding+LineEnding+
    'Application for personal use only under the GNU GENERAL PUBLIC LICENSE Version 3.';

  // Initial LCD has 4 rows and 20 cols - LCD 20x04
  LCD_ROWS                    = 4;
  LCD_COLS                    = 20;
  LCD_CHAR_IMAGE_SIZE         = 5;            // Default size of the LCD chars
  LCD_CHAR_COLS               = 5;            // LCD Char 5cols x 8rows
  LCD_CHAR_ROWS               = 8;
  LCD_CHARWIDTH               = 30;
  LCD_CHARHEIGHT              = 40;
  LCD_PREVIEW_BORDER_WIDTH    = 10;           // 5 is also nice
  LCD_DELIMITER               = ':';
  LCD_EDIT_NORMALCHAR_COLOR   = clDefault;
  LCD_EDIT_CUSTOMCHAR_COLOR   = clBlue;
  LCD_EDIT_EXTENDEDCHAR_COLOR = clRed;
  LCD_EDIT_NORMALCHAR         = 'N';
  LCD_EDIT_CUSTOMCHAR         = 'C';
  LCD_EDIT_EXTENDEDCHAR       = 'X';

  // Table char
  // index ranges
  // C=0-7;N=32-127;X=128-255
  LCD_CHAR_CUSTOM_INDEX_MIN   = 0;
  LCD_CHAR_CUSTOM_INDEX_MAX   = 7;
  LCD_CHAR_NORMAL_INDEX_MIN   = 32;
  LCD_CHAR_NORMAL_INDEX_MAX   = 127;
  LCD_CHAR_EXT_INDEX_MIN      = 128;
  LCD_CHAR_EXT_INDEX_MAX      = 255;
  // File holding all table chars 0-255
  // index:name:pattern
  FILE_TABLE_CHARS            = 'tablechars.def';
  LCD_CHAR_NONAME             = 'NN';
  LCD_CHAR_PATTERN_EMPTY      = '0,0,0,0,0,0,0,0';
  LCD_FORMAT_BIN_BITS         = 5;    //

  // Number of LCD display with their index
  // LCD1602=0, LCD2004=1, LCD1603=2, LCD0802=3
  LCDDISPLAYS                 = 4;
  FILE_DISPLAY_DEFINITION_EXT = '.dsp';

  // Code Types & Code Templates
  CODE_CPP                 = 'CPP';
  CODE_TEMPLATE_CPP        = 'lcdddex.pde';
  CODE_B4R                 = 'B4R';
  CODE_TEMPLATE_B4R        = 'lcdddex.b4r';
  CODE_PYTHON_TF           = 'Python Tinkerforge';
  CODE_TEMPLATE_PYTHON_TF  = 'lcdddex.py';
  CODE_TYPES_ALL           : Array of String = (CODE_B4R, CODE_CPP, CODE_PYTHON_TF);
  CODE_CHAR_TAB            = Chr(9);

type
  // CharTypes with table index: N)ormal 32-127,C)ustom 0-7,X)xtended 128-255
  TLCDCharTypes = (TypeNormal, TypeCustom, TypeExtended);

  TLCDTableChar = Record
      CharIndex: Integer;      //0-7:Custom Chars; 8-255:Chars
      CharName: String;        //Battery; A,B...
      CharType: TLCDCharTypes;
      CharPattern: String;     //Battery=14,27,17,17,17,17,17,31;A=14,17,17,17,31,17,17,0;
  end;
  // All 255 possible chars to display - loaded from file tablechars.def
  TLCDTableChars = Array[0..255] of TLCDTableChar;

  // Definition of a char display at certain row:col
  TLCDChar = Record
    Row: Integer;             // 1 .. 4 - 20x4, 16x2
    Col: Integer;             // 1 .. 20 - see above
    Char: TLCDTableChar;
  end;
  TLCDChars = Array[1..LCD_ROWS, 1..LCD_COLS] of TLCDChar;

  TLCDColorScheme = Record
    Name: String;
    PixelHigh: TColor;
    PixelLow: TColor;
    Border: TColor;
  end;

const
  // LCD Preview color schemes
  // Name (LowHigh), PixelHigh (state=1), PixelLow (state=0, background), Border (small border underneath black main border
  COLORSCHEMES: Array[1..6] of TLCDColorScheme = (
    (Name:'GreenBlack'; PixelHigh:clBlack;  PixelLow:TColor($7FFF00); Border: TColor($7FFF00)),
    (Name:'WhiteRed';   PixelHigh:clRed;    PixelLow:clWhite; Border: clWhite),
    (Name:'RedWhite';   PixelHigh:clWhite;  PixelLow:clRed; Border: clRed),
    (Name:'BlueBlack';  PixelHigh:clWhite;  PixelLow:clNavy; Border: clNavy),
    (Name:'YellowBlack';PixelHigh:clBlack;  PixelLow:clYellow; Border: clYellow),
    (Name:'WhiteBlack'; PixelHigh:clBlack;  PixelLow:clWhite; Border: clWhite)
    );

  type
  TLCDDisplay = Record
    Name: String;             // 'LCD 20x04', 'LCD 16x02'
    Index: Integer;           // 0, 1 - used for the combobox display selection
    Rows: Integer;            // 4, 2 - 20x4, 16x2
    Cols: Integer;            // 20, 16 - see above
    Address: String;          // '0x27', '0x3F'
    Chars: TLCDChars;
  end;

  { TFormMain }
  TFormMain = class(TForm)
    ActionToolsSetListTableChars: TAction;
    ActionToolsLoadTableChars: TAction;
    ActionToolsInsertTableChar: TAction;
    ActionCodePythonTinkerforge: TAction;
    ActionToolsViewTableChars: TAction;
    ActionToolsConvertTableChar: TAction;
    ActionFileQuickSave: TAction;
    ActionCodeCPP: TAction;
    ActionCodeB4R: TAction;
    ActionEditRemoveChar: TAction;
    ActionToolsInsertCustomChar: TAction;
    ActionCodeExample: TAction;
    ActionEditClearChars: TAction;
    ActionHelpAbout: TAction;
    ActionFileSave: TAction;
    ActionFileOpen: TAction;
    ActionCreateLCD: TAction;
    ActionFileExit: TAction;
    ActionListMain: TActionList;
    ComboBoxCodeExample: TComboBox;
    ComboBoxLCDSelected: TComboBox;
    EditLCDName: TEdit;
    GroupBoxLCDPreviewOptions: TGroupBox;
    GroupBoxLCDTableChars: TGroupBox;
    ImageLCDPreview: TImage;
    ImageLCDTableCharPreview: TImage;
    ImageListMain: TImageList;
    LabelLCDPreviewColorSchemes: TLabel;
    ListBoxLCDPreviewColorScheme: TListBox;
    MainMenuMain: TMainMenu;
    MenuFile: TMenuItem;
    MenuFileOpen: TMenuItem;
    MenuFileSave: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem5: TMenuItem;
    MenuEdit: TMenuItem;
    N2: TMenuItem;
    MenuHelp: TMenuItem;
    N1: TMenuItem;
    MenuItemFileExit: TMenuItem;
    OpenDialogMain: TOpenDialog;
    PanelTableCharsToolbar: TPanel;
    PanelLCD: TPanel;
    PanelLCDPreview: TPanel;
    PanelLCDTableChars: TPanel;
    PanelTop: TPanel;
    PanelLCDCreate: TPanel;
    SaveDialogMain: TSaveDialog;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButtonCodeExample: TSpeedButton;
    SplitterTableChars: TSplitter;
    SplitterLCDPreview: TSplitter;
    StatusBarMain: TStatusBar;
    StringGridLCDTableChars: TStringGrid;
    procedure ActionCodeB4RExecute(Sender: TObject);
    procedure ActionCodeCPPExecute(Sender: TObject);
    procedure ActionCodePythonTinkerforgeExecute(Sender: TObject);
    procedure ActionCreateLCDExecute(Sender: TObject);
    procedure ActionEditClearCharsExecute(Sender: TObject);
    procedure ActionFileExitExecute(Sender: TObject);
    procedure ActionFileOpenExecute(Sender: TObject);
    procedure ActionFileQuickSaveExecute(Sender: TObject);
    procedure ActionFileSaveExecute(Sender: TObject);
    procedure ActionHelpAboutExecute(Sender: TObject);
    procedure ActionCodeExampleExecute(Sender: TObject);
    procedure ActionToolsConvertTableCharExecute(Sender: TObject);
    procedure ActionToolsInsertTableCharExecute(Sender: TObject);
    procedure ActionToolsLoadTableCharsExecute(Sender: TObject);
    procedure ActionToolsViewTableCharsExecute(Sender: TObject);
    procedure ActionEditRemoveCharExecute(Sender: TObject);
    procedure EditLCDNameEnter(Sender: TObject);
    procedure EditLCDNameKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ComboBoxLCDSelectedChange(Sender: TObject);
    procedure ListBoxLCDPreviewColorSchemeClick(Sender: TObject);
    procedure StringGridLCDTableCharsClick(Sender: TObject);
    procedure StringGridLCDTableCharsDblClick(Sender: TObject);
  private
    FLCDTableChars: TLCDTableChars;
    FLCDDisplays: Array [0..LCDDISPLAYS - 1] of TLCDDisplay;
    FLCDSelected: TLCDDisplay;
    FLCDEditSelected: TEdit;
    FRow: Integer;
    FCol: Integer;
    FLoading: Boolean;
    FLCDColorScheme: Integer;
    // LCD definitions
    procedure CreateLCD(Sender: TObject);
    procedure SetLCDFormat(Sender: TObject);
    Procedure LoadLCDJSON(Sender: TObject;AFileName: String);
    Procedure SaveLCDJSON(Sender: TObject;AFileName: String);
    Procedure ClearLCD(Sender: TObject);

    // Char Edit Fields
    procedure PanelLCDEditClick(Sender: TObject);
    procedure PanelLCDEditEnter(Sender: TObject);
    procedure PanelLCDEditExit(Sender: TObject);
    procedure PanelLCDEditChange(Sender: TObject);
    procedure PanelLCDEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);

    Procedure SetLCDEditFocus(ARow, ACol: Integer);
    Procedure GetLCDEditPos(AEdit: TEdit; Var ARow, ACol: Integer);
    Function  GetLCDEdit(ARow, ACol: Integer):TEdit;      // NOT USED = Keep for future
    Procedure SetLCDEditChar(ARow, ACol: Integer; ACharType: TLCDCharTypes; AChar, AHint: String);
    Function  GetLCDEditChar(ARow, ACol: Integer): String;   // NOT USED = Keep for future
    Procedure GetLCDControlPos(AControl: TControl; Var ARow, ACol: Integer);

    // LCD Matrix 2 dim array
    Function  InitLCDDisplay(AName:String;AIndex:Integer;AAddress:String;ARows,ACols:Integer):TLCDDisplay;
    Function  InitLCDChars(ARows,ACols: Integer):TLCDChars;
    Function  GetLCDChar(ALCDDisplay:TLCDDisplay;ARow,ACol:Integer):TLCDChar;

    Function  GetLCDTableCharFromDefinition(ACharDefinition: String):TLCDTableChar;
    Function  GetLCDTableCharFromIndex(AIndex: Integer):TLCDTableChar;
    Function  GetLCDTableCharName(AIndex: Integer):String;
    Function  IsLCDCustomChar(ALCDDisplay:TLCDDisplay;ARow,ACol:Integer):Boolean;

    Function  GetCharType(AIndex:Integer):TLCDCharTypes;
    Function  SetCharTypeStr(ACharType: TLCDCharTypes): String;

    // LCD Table Chars (ValueListEditor)
    procedure SetLCDTableCharsIndex(Sender:TObject; AIndex: Integer);
    function  GetLCDTableCharsIndex(Sender:TObject):Integer;

    // Images
    procedure DrawImageLCDTableChar(AImage: TImage; APattern: String);
    procedure ClearImageLCDTableChar(AImage: TImage);
    procedure DrawImageLCDPreview(AImage: TImage);
    procedure ClearImageLCDPreview(AImage: TImage);

    // LCD Preview Color Schemes
    Procedure SetLCDPreviewColorSchemeSelection(Sender: TObject);

    // Other Forms
    Procedure ShowCodeViewer(Sender: TObject;ACodeType: String; ACode: String);

    // Helpers
    Procedure SetStatusBar(ARow, ACol: Integer; AChar, AHint: String);
    Procedure SetStatusBarChar;

  public
    //
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

//
// FORMMAIN
//
procedure TFormMain.FormCreate(Sender: TObject);
var
  i: Integer;
begin
 FLoading := True;
 // LCD Displays
 FLCDDisplays[0] := InitLCDDisplay('LCD1602',0,'0x3F',2,16);
 FLCDDisplays[1] := InitLCDDisplay('LCD2004',1,'0x27',4,20);
 FLCDDisplays[2] := InitLCDDisplay('LCD1603',2,'ADDRESS',3,16);
 FLCDDisplays[3] := InitLCDDisplay('LCD0801',3,'ADDRESS',1,8);
 // Set LCD2004 as default (index 1)
 FLCDSelected := FLCDDisplays[1];
 // Load the LCD table chars into the selection listbox
 ActionToolsLoadTableCharsExecute(Sender);
 // Add combobox items for the example code and lcd selection
 ComboBoxCodeExample.Items.Clear;
 ComboBoxCodeExample.Items.Add(CODE_B4R);
 ComboBoxCodeExample.Items.Add(CODE_CPP);
 ComboBoxCodeExample.Items.Add(CODE_PYTHON_TF);
 ComboBoxCodeExample.ItemIndex := 0;
 // Add the LCDs to the selection combo box
 ComboBoxLCDSelected.Items.Clear;
 for i := 0 to LCDDISPLAYS - 1 do
   ComboBoxLCDSelected.Items.Add(FLCDDisplays[i].Name);
 ComboBoxLCDSelected.ItemIndex := FLCDSelected.Index;
 // Create the LCD with default 20x04 - DO NOT CHANGE
 CreateLCD(Sender);
 // Set the LCD preview color scheme
 SetLCDPreviewColorSchemeSelection(Sender);
 FLoading := False;
end;

// Set first edit field as focus
procedure TFormMain.FormShow(Sender: TObject);
begin
 Caption := APPTITLEVERSION;
 // Set control sizes
 FormMain.Width := 1050;
 FormMain.Height := 550;
 PanelLCDTableChars.Width := 200;
 PanelLCDPreview.Height := 200;
 // Show LCD table char preview
 StringGridLCDTableCharsClick(Sender);
 // Set first char edit field
 FRow := 1; FCol := 1;
 SetLCDEditFocus(FRow,FCol);
end;

(*
  Set the LCD preview color scheme.
  This procedure is also called during form create.
*)
procedure TFormMain.ListBoxLCDPreviewColorSchemeClick(Sender: TObject);
begin
  FLCDColorScheme := ListBoxLCDPreviewColorScheme.ItemIndex + 1;
  DrawImageLCDPreview(ImageLCDPreview);
end;

(*
  Show the char pixel image when click on stringgrid lcd table chars.
*)
procedure TFormMain.StringGridLCDTableCharsClick(Sender: TObject);
begin
if FLoading then Exit;
with StringGridLCDTableChars do begin
  if Row < 0 then exit;
  try
    DrawImageLCDTableChar(
      ImageLCDTableCharPreview,
      FLCDTableChars[StrToInt(Cells[1,Row])].CharPattern);
  except on E: Exception do
    ShowError(
       'View LCD Table Char',
       E.Message + LineEnding + FLCDTableChars[StrToInt(Cells[1,Row])].CharName);
    end;
  end;
end;

procedure TFormMain.StringGridLCDTableCharsDblClick(Sender: TObject);
begin

end;

//
// ACTIONS
//
procedure TFormMain.ActionCreateLCDExecute(Sender: TObject);
begin
  CreateLCD(Sender);
end;

procedure TFormMain.ActionFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.ActionFileOpenExecute(Sender: TObject);
  function GetFilename(f: String):String;
  begin
   GetFilename := ExtractFileName(f.Substring(1, pos('.',f) - 2));
  end;
begin
  if OpenDialogMain.Execute then begin
    if fileExists(OpenDialogMain.Filename) then begin
      try
        LoadLCDJSON(Sender, OpenDialogMain.Filename);
        EditLCDName.Text := GetFileName(OpenDialogMain.Filename);
        DrawImageLCDPreview(ImageLCDPreview);
        SetStatusBarSimpleText(StatusBarMain, Format(
          'LCD display definition loaded: %s',
          [OpenDialogMain.Filename]));
        EditLCDName.Hint := OpenDialogMain.Filename;
      except on E: Exception do
         ShowError(
          'Load LCD Display Definition',
          E.Message + LineEnding + OpenDialogMain.Filename);
      end;
    end;
  end;
end;

procedure TFormMain.ActionFileSaveExecute(Sender: TObject);
var
  FileName: String;
begin
  // Check if name entered
  if Length(EditLCDName.Text) = 0 then begin
    ShowError( 'Save LCD Display Definition', Format(
      '%s',
      ['No name defined.']));
    SetStatusBarSimpleText(StatusBarMain, Format(
      'LCD display definition NOT saved. No name defined.',
      []));
  end
  else begin
    SaveDialogMain.FileName := EditLCDName.Text;
    if SaveDialogMain.Execute then begin
      try
        filename := ExtractFileName(SaveDialogMain.FileName);
        EditLCDName.Hint := SaveDialogMain.FileName;
        EditLCDName.Text := StringReplace(filename, FILE_DISPLAY_DEFINITION_EXT,'',[rfReplaceAll,rfIgnoreCase]);
        SaveLCDJSON(Sender, SaveDialogMain.FileName);
        SetStatusBarSimpleText(StatusBarMain, Format(
          'LCD display definition saved: %s',
          [SaveDialogMain.FileName]));
      except on E: Exception do
         ShowError(
          'Save LCD Display Definition',
          E.Message + LineEnding + OpenDialogMain.Filename);
      end;
    end else begin
      EditLCDName.Hint := '';
    end;
  end;
end;

procedure TFormMain.ActionFileQuickSaveExecute(Sender: TObject);
var
  FileName: String;
begin
  // Check if name entered
  if Length(EditLCDName.Text) = 0 then begin
    ShowError('Save LCD Display Definition', Format(
      '%s',
      ['No name defined.']));
    SetStatusBarSimpleText(StatusBarMain, Format(
      'LCD Display Definition not saved. No name defined.',
      []));
    EditLCDName.Hint := '';
  end
  else begin
    FileName := EditLCDName.Text + FILE_DISPLAY_DEFINITION_EXT;
    SaveLCDJSON(Sender, FileName);
  end;
end;

(*
  Remove a char from the lcd display array
*)
procedure TFormMain.ActionEditRemoveCharExecute(Sender: TObject);
begin
 with FLCDSelected.Chars[FRow,FCol].Char do begin
   CharIndex    := -1;
   CharName     := '';
   CharType     := TypeNormal;
   CharPattern  := '';
 end;
 with FLCDEditSelected do begin
   Text := '';
   Hint := '';
   Font.Color := LCD_EDIT_NORMALCHAR_COLOR;
  end;
 ClearImageLCDTableChar(ImageLCDTableCharPreview);
 DrawImageLCDPreview(ImageLCDPreview);
 SetStatusBar(FRow, FCol, FLCDEditSelected.Text, FLCDEditSelected.Hint);
end;

procedure TFormMain.EditLCDNameEnter(Sender: TObject);
begin
  // Save the lcd definition
 ActionFileSaveExecute(Sender);
end;

procedure TFormMain.EditLCDNameKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) then begin end;
  if Key = VK_RETURN then
    ActionFileSaveExecute(Sender);
end;

procedure TFormMain.ActionEditClearCharsExecute(Sender: TObject);
begin
  ClearLCD(Sender);
end;

//
// CODE GENERATE & VIEW EXAMPLE
//

procedure TFormMain.ActionCodeExampleExecute(Sender: TObject);
begin
 case ComboBoxCodeExample.Items[ComboBoxCodeExample.ItemIndex] of
   CODE_B4R : ActionCodeB4RExecute(Sender);
   CODE_CPP : ActionCodeCPPExecute(Sender);
   CODE_PYTHON_TF: ActionCodePythonTinkerforgeExecute(Sender);
 end;
end;

procedure TFormMain.ActionCodeB4RExecute(Sender: TObject);
const
  CHAR_B4R_COMMENT = Chr(39);
var
  LCDText: String;
  LCDCustomCharsDefine: String;
  LCDCustomCharsCreate: String;
  Code: String;
  CodeComment: String;
  sl: TStringList;
  Row, Col: Integer;
  i: Integer;
begin
 // Get the code for the LCD rows and assign to the LCDText
 LCDText := '';
 LCDCustomCharsDefine := '';
 LCDCustomCharsCreate := '';
 // Create custom chars
 for i:=0 to LCD_CHAR_CUSTOM_INDEX_MAX do begin
   with FLCDTableChars[i] do begin
      // Private arraybatteryfull() As Byte = Array As Byte (0x0E,0x1F,0x1F,0x1F,0x1F,0x1F,0x1F,0x1F)
      LCDCustomCharsDefine := LCDCustomCharsDefine + CODE_CHAR_TAB + Format(
        'Private %s() As Byte = Array As Byte (%s)',
        [CharName, CharPattern]) + LineEnding;
      // lcd.CreateChar(locationbatteryfull, arraybatteryfull)
      LCDCustomCharsCreate := LCDCustomCharsCreate + CODE_CHAR_TAB + Format(
        'lcd.CreateChar(%d, %s)',
        [CharIndex, CharName]) + LineEnding;
   end;
 end;
 // Text
 for Row := 1 to FLCDSelected.Rows do begin
     for Col := 1 to FLCDSelected.Cols do begin
       with FLCDSelected.Chars[Row, Col].Char do begin
         if Length(CharName.Trim) > 0 then begin
           CodeComment := CODE_CHAR_TAB + Format(
            '%s %s',
            [CHAR_B4R_COMMENT, GetLCDTableCharName(CharIndex)]);
           if CharIndex > LCD_CHAR_CUSTOM_INDEX_MAX then begin
              LCDText := LCDText + CODE_CHAR_TAB + Format(
              'lcd.WriteAt(%d, %d, Array As Byte(%d)) %s',
              [Col - 1, Row - 1, CharIndex, CodeComment]) + LineEnding;
           end;
           if CharIndex < 8 then begin
             LCDText := LCDText + CODE_CHAR_TAB + Format(
               'lcd.WriteCharAt(%d, %d, %d) %s',
               [Col - 1, Row - 1, CharIndex, CodeComment]) + LineEnding;
           end;
         end;
       end;
     end;
 end;
 try
    sl := TStringList.Create;
    sl.LoadFromFile(CODE_TEMPLATE_B4R);
    sl.Text := sl.Text.Replace('#LCDCUSTOMCHARSDEFINE#', LCDCustomCharsDefine);
    sl.Text := sl.Text.Replace('#LCDCUSTOMCHARSCREATE#', LCDCustomCharsCreate);
    sl.Text := sl.Text.Replace('#LCDTEXT#', LCDText);
    Code := sl.Text;
    sl.Free;
    // Assign the code example to synedit and show the form
    ShowCodeViewer(
      Sender,
      ComboBoxCodeExample.Items[ComboBoxCodeExample.ItemIndex],
      Code);
 except on E: Exception do
    ShowError(Format(
       'Create Code - %s',[ComboBoxCodeExample.Items[ComboBoxCodeExample.ItemIndex]]),
       E.Message);
 end;
end;

procedure TFormMain.ActionCodeCPPExecute(Sender: TObject);
const
  CHAR_CPP_COMMENT = '//';
var
  LCDText: String;
  LCDCustomCharsDefine: String;
  LCDCustomCharsCreate: String;
  Code: String;
  CodeComment: String;
  sl: TStringList;
  Row, Col: Integer;
  i: Integer;
begin
 // Get the code for the LCD rows and assign to the LCDText
 LCDText := '';
 LCDCustomCharsDefine := '' +
   '// Define printByte' + LineEnding +
   '#if defined(ARDUINO) && ARDUINO >= 100' + LineEnding +
   '#define printByte(args) write(args);' + LineEnding +
   '#else' + LineEnding +
   '#define printByte(args) print(args,BYTE);' + LineEnding +
   '#endif' + LineEnding;
 LCDCustomCharsCreate := '';
 // Create custom chars
 for i:=LCD_CHAR_CUSTOM_INDEX_MIN to LCD_CHAR_CUSTOM_INDEX_MAX do begin
   with FLCDTableChars[i] do begin
     if CharIndex < 8 then begin
       // byte smiley[8] = {B00000,B10001,B00000,B00000,B10001,B01110,B00000};
       LCDCustomCharsDefine := LCDCustomCharsDefine + Format(
         'uint8_t %s[8] = {%s};',
         [CharName, CharPattern]) + LineEnding;
       // void setup() { lcd.createChar(0, smiley); ...
       LCDCustomCharsCreate := LCDCustomCharsCreate + CODE_CHAR_TAB + Format(
         'lcd.createChar(%d,%s);',
         [CharIndex, CharName]) + LineEnding;
     end;
   end;
 end;
 // Text
 for Row := 1 to FLCDSelected.Rows do begin
     for Col := 1 to FLCDSelected.Cols do begin
       with FLCDSelected.Chars[Row, Col].Char do begin
         if Length(CharName.Trim) > 0 then begin
           CodeComment := CODE_CHAR_TAB + Format(
            '%s %s',
            [CHAR_CPP_COMMENT, GetLCDTableCharName(CharIndex)]);
           // setCursor(Col, Row)
           LCDText := LCDText + CODE_CHAR_TAB + Format(
             'lcd.setCursor(%d, %d);',
             [Col - 1, Row - 1]) + LineEnding;
           // All characters are printed as bytes
           LCDText := LCDText + CODE_CHAR_TAB + Format(
             'lcd.printByte(%d); %s',
             [CharIndex, CodeComment]) + LineEnding;
         end;
       end;
     end;
 end;
 try
   sl := TStringList.Create;
   sl.LoadFromFile(CODE_TEMPLATE_CPP);
   sl.Text := sl.Text.Replace('#LCDCUSTOMCHARSDEFINE#', LCDCustomCharsDefine);
   sl.Text := sl.Text.Replace('#LCDCUSTOMCHARSCREATE#', LCDCustomCharsCreate);
   sl.Text := sl.Text.Replace('#LCDTEXT#', LCDText);
   Code := sl.Text;
   sl.Free;
   // Assign the code example to synedit and show the form
   ShowCodeViewer(
     Sender,
     ComboBoxCodeExample.Items[ComboBoxCodeExample.ItemIndex],
     Code);
 except on E: Exception do
    ShowError(Format(
       'Create Code - %s',[ComboBoxCodeExample.Items[ComboBoxCodeExample.ItemIndex]]),
       E.Message);
 end;
end;

procedure TFormMain.ActionCodePythonTinkerforgeExecute(Sender: TObject);
const
  CHAR_PYTHON_COMMENT = '##';
  CODE_PYTHON_TAB = '    ';  // Use spaces instead Chr(9)
var
  LCDText: String;
  LCDCustomCharsDefine: String;
  LCDCustomCharsCreate: String;
  Code: String;
  CodeComment: String;
  sl: TStringList;
  Row, Col: Integer;
  i: Integer;

begin
 // Get the code for the LCD rows and assign to the LCDText
 LCDText := '';
 LCDCustomCharsDefine := '';
 LCDCustomCharsCreate := '';
 // Create custom chars
 for i:=LCD_CHAR_CUSTOM_INDEX_MIN to LCD_CHAR_CUSTOM_INDEX_MAX do begin
   with FLCDTableChars[i] do begin
     if CharIndex <= LCD_CHAR_CUSTOM_INDEX_MAX then begin
       // byte smiley[8] = {B00000,B10001,B00000,B00000,B10001,B01110,B00000};
       LCDCustomCharsDefine := LCDCustomCharsDefine + CODE_PYTHON_TAB + Format(
         '%s = [%s]',
         [CharName, CharPattern]) + LineEnding;
       // void setup() { lcd.createChar(0, smiley); ...
       LCDCustomCharsCreate := LCDCustomCharsCreate + CODE_PYTHON_TAB + Format(
         'lcd.set_custom_character(%d, %s)',
         [CharIndex, CharName]) + LineEnding;
     end;
   end;
 end;
 // Text
 for Row := 1 to FLCDSelected.Rows do begin
     for Col := 1 to FLCDSelected.Cols do begin
       with FLCDSelected.Chars[Row, Col].Char do begin
         if Length(CharName.Trim) > 0 then begin
           CodeComment := CODE_PYTHON_TAB + Format(
            '%s %s',
            [CHAR_PYTHON_COMMENT, GetLCDTableCharName(CharIndex)]);
           // All characters are printed as bytes
           if CharIndex > LCD_CHAR_CUSTOM_INDEX_MAX then
             LCDText := LCDText + CODE_PYTHON_TAB + Format(
               'lcd.write_line(%d, %d, "\x%s") %s',
               [Row - 1, Col - 1, IntToHex(CharIndex,1), CodeComment]) + LineEnding;
           if CharIndex <= LCD_CHAR_CUSTOM_INDEX_MAX then
             LCDText := LCDText + CODE_PYTHON_TAB + Format(
             'lcd.write_line(%d, %d, "\x0%s") %s',
             [Row - 1, Col - 1, IntToHex(CharIndex + 8, 1), CodeComment]) + LineEnding;
         end;
       end;
     end;
 end;
 try
   sl := TStringList.Create;
   sl.LoadFromFile(CODE_TEMPLATE_PYTHON_TF);
   sl.Text := sl.Text.Replace('#LCDCUSTOMCHARSDEFINE#', LCDCustomCharsDefine);
   sl.Text := sl.Text.Replace('#LCDCUSTOMCHARSCREATE#', LCDCustomCharsCreate);
   sl.Text := sl.Text.Replace('#LCDTEXT#', LCDText);
   sl.Text := sl.Text.Replace(CODE_CHAR_TAB, CODE_PYTHON_TAB);
   Code := sl.Text;
   sl.Free;
   // Assign the code example to synedit and show the form
   ShowCodeViewer(
     Sender,
     ComboBoxCodeExample.Items[ComboBoxCodeExample.ItemIndex],
     Code);
 except on E: Exception do
    ShowError(Format(
       'Create Code - %s',[ComboBoxCodeExample.Items[ComboBoxCodeExample.ItemIndex]]),
       E.Message);
 end;
end;

procedure TFormMain.ShowCodeViewer(Sender: TObject; ACodeType: String; ACode: String);
begin
 with FormCodeViewer do begin
   Case ACodeType of
     CODE_B4R: SynEditCode.Highlighter := SynB4RSyn;
     CODE_CPP: SynEditCode.Highlighter := SynCPPSyn;
     CODE_PYTHON_TF: SynEditCode.Highlighter := SynPythonSyn;
   end;
   CodeType := ACodeType;
   SynEditCode.Lines.Text := ACode;
   Height := FormMain.Height;
   Width := FormMain.Width;
   ShowModal;
 end;
end;

//
// TABLE CHARS
//

(*
  Load table chars from definition file (tablechars.def) into:
  Array FLCDTableChars and the lcdlistbox selection.
  ListBoxLCDTableChars add chars but only if pattern is not empty 0,0,0...
  Types are C=Custom Char (Index 0-7), T=Table Char (Index 10 or higher).
  NOTE:
  A table char is converted from bin to hex.
  Example:
  Degree is col 1101, row 1111. 11011111 is converted to DF (dec 223)
  Format: Index:Name:Pattern - 223:Degree:28,20,28, 0, 0, 0, 0
*)
procedure TFormMain.ActionToolsLoadTableCharsExecute(Sender: TObject);
var
  sl: TStringList;
  i: Integer;
begin
  try
    // Load the table chars file into the lcdtablechars array
    sl := TStringList.Create;
    sl.LoadFromFile(FILE_TABLE_CHARS);
    sl := StringListDeleteEmptyLines(sl);
    for i:= 0 to sl.Count - 1 do begin
      FLCDTableChars[i] := GetLCDTableCharFromDefinition(sl[i]);
    end;
    sl.Free;
    with StringGridLCDTableChars do begin
      Clear;
      RowCount := 1;
      for i:= 0 to Length(FLCDTableChars) - 1 do begin
        with FLCDTableChars[i] do begin
         if (CharPattern <> LCD_CHAR_PATTERN_EMPTY) and
            (CharName <> LCD_CHAR_NONAME) then begin
              InsertRowWithValues(RowCount,[CharName,IntToStr(CharIndex)]);
            end;
        end;
      end;
    end;

    // WORKAROUND = Hande special characters ;, = as not properly handled by split
    { TODO 4 -cChar : Handle workaround for special characters. Instead names like semicolon, equal use ;, = characters BUT these are either not accepted by the LCDValueListTable or ia the split function. }
    for i:= 0 to Length(FLCDTableChars) - 1 do begin
       Case i of
         59: FLCDTableChars[i].CharName := ';';
         61: FLCDTableChars[i].CharName := '=';
        end;
    end;
  except on E: Exception do
     ShowError(Format(
        'Load Table Chars - %s',[FILE_TABLE_CHARS]),
        E.Message);
  end;
end;

(*
  Insert a table char selected from the special char listbox.
  NOTE: After insert there is a short delay to enable message processing.
  (Check this is required at least under linux using listbox doubleclick event)
  A table char consists out of a colon (:) delimited string with 4 items.
  name(0):type(1):index(2):value (3), i.e. array index 0,1,2,3.
  Table char: decimal value taken from the table row col byte.
  Examples Table Char (T):
  Degree:T:10:223
  name=Degree,type=T,index=10,value=223
  DoubleQuote:T:11:34
  The table char value is converted from the lcd display table:
  Example Degree °: col=1101, row=1111, convert 1101111 to HEX=DF, Int=223
  Example DoubleQuote ": col=0010, row=0010, convert 00100010 to HEX=22, Int=34
*)
procedure TFormMain.ActionToolsInsertTableCharExecute(Sender: TObject);
var
  index: Integer;
begin
  with StringGridLCDTableChars do begin
    index := StrToInt(Cells[1, Row]);
  end;
  // Assign the char properties to the lcd selected char array matrix
 with FLCDSelected.Chars[FRow,FCol].Char do begin
   CharName    := FLCDTableChars[index].CharName;
   CharIndex   := FLCDTableChars[index].CharIndex;
   CharType    := GetCharType(CharIndex);
   CharPattern := FLCDTableChars[index].CharPattern;
   DrawImageLCDTableChar(ImageLCDTableCharPreview, CharPattern);
   // Set the selected edit field
   with FLCDEditSelected do begin
     Text := CharName;
     case CharType of
       TypeNormal: Font.Color := LCD_EDIT_NORMALCHAR_COLOR;
       TypeCustom: Font.Color := LCD_EDIT_CUSTOMCHAR_COLOR;
       TypeExtended: Font.Color := LCD_EDIT_EXTENDEDCHAR_COLOR;
     end;
   end;
 end;
 Delay(50);
 FLCDEditSelected.SetFocus;
 FLCDEditSelected.SelStart := 0;
end;

(*
  Convert table char bits to HEX and DEC Value
*)
procedure TFormMain.ActionToolsConvertTableCharExecute(Sender: TObject);
var
  BitStr: String;
  HexValue: String;
  IntValue: Integer;
  AscIIValue: String;
  Result: String;
begin
  BitStr := '00100010';
  if InputQuery('Convert Table Char Bits', 'Enter Bit String', False, BitStr) then begin
   if Length(BitStr) = 8 then begin
    HexValue := ByteStringToHex(BitStr);
    IntValue := ByteStringToInt(BitStr);
    AscIIValue := HexToChar(HexValue);
    Result := Format(
      'DEC: %d HEX: %s ASCII: %s',
      [IntValue,HexValue,AscIIValue]);
   ClipBoard.AsText := Result;
   ShowInfo(
     Format('Convert Table Char Bits - %s',[BitStr]),
     Format('%s',[Result]));
   end
   else begin
     ShowError(
       Format('Convert Table Char Bits - %s',[BitStr]),
       Format('%s',['The Bit string must contain 8 Bits.']));
   end
  end
end;

procedure TFormMain.ActionToolsViewTableCharsExecute(Sender: TObject);
begin
  with FormLCDTableChars do begin
    ShowModal;
  end;
end;

//
// HELP
//

procedure TFormMain.ActionHelpAboutExecute(Sender: TObject);
begin
  ShowInfo('About ...', APPDESCRIPTION);
end;

//
// VARIOUS CONTROLS
//
procedure TFormMain.ComboBoxLCDSelectedChange(Sender: TObject);
begin
  if ComboBoxLCDSelected.ItemIndex > -1 then begin
   // ItemIndex:0=16x02,1=20x04,...
   FLCDSelected := FLCDDisplays[ComboBoxLCDSelected.ItemIndex];
   SetLCDFormat(Sender);
   FRow := 1; FCol := 1;
   SetLCDEditFocus(FRow,FCol);
  end;
end;

//
// CHAR MATRIX
//
(*
  Set the LCD format depending selected display FLCDSelected.
*)
procedure TFormMain.SetLCDFormat(Sender: TObject);
var
  AControl: TControl;
  Row,Col: Integer;
  n: Integer;
begin
 Row := -1;
 Col := -1;
 FormMain.BeginFormUpdate;
 // Clear all fields
 ClearLCD(Sender);
 // Hide all controls
 for n := 0 to PanelLCDCreate.ControlCount - 1 do begin
   PanelLCDCreate.controls[n].Visible := False;
 end;
 // Loop over all chars and make visible if cols/rows within selected display
 for n := 0 to PanelLCDCreate.controlcount - 1 do begin
   AControl := PanelLCDCreate.controls[n];
   // Get the row,col pos of the control into Row, Col
   GetLCDControlPos(AControl, Row, Col);
   // Set property Visible
   AControl.Visible := ((Row <= FLCDSelected.Rows) and (Col <= FLCDSelected.Cols));
 end;
 FormMain.EndFormUpdate;
end;

(*
  Create the character matrix with a panel for each char.
  The matrix has 8 rows with 5 cols.
  Each char bit panel is identified by its id row:col assigned to the hint.
  The panel tag sets the state high (1, blue) or low (0, white).
*)
procedure TFormMain.CreateLCD(Sender: TObject);
var
  RowNr,ColNr: Integer;

  procedure CreateMatrixLabel(row, col: Integer; l: Integer; t: Integer; c: String);
  Var
    LabelMatrix: TLabel;
  begin
    LabelMatrix := TLabel.Create(self);
    With LabelMatrix do begin
      Left        := l;
      Top         := t;
      Width       := LCD_CHARWIDTH;
      Height      := LCD_CHARHEIGHT;
      AutoSize    := False;
      BorderStyle := bsNone;
      Caption     := c;
      Font.Style  := Font.Style - [fsBold];
      Alignment   := taCenter;
      Layout      := tlCenter;
      Color       := clNone;
      Hint        := '';
      Tag         := StrToInt(Format('%d%d',[row,col]));
      Parent      := PanelLCDCreate;
      BorderStyle := bsSingle;
      BorderWidth := 0;
      ShowHint    := False;
   end;
  end;

  procedure CreateCharEdit(row, col: Integer; l: Integer; t: Integer);
  Var
    EditChar: TEdit;
  begin
    EditChar := TEdit.Create(self);
    With EditChar do begin
      Left        := l;
      Top         := t;
      Width       := LCD_CHARWIDTH;
      Height      := LCD_CHARHEIGHT;
      BorderStyle := bsNone;
      Caption     := '';
      Text        := #0;
      Font.Style  := Font.Style + [fsBold];
      Alignment   := taCenter;
      MaxLength   := 1;
      Color       := LCD_EDIT_NORMALCHAR_COLOR;
      Hint        := '';
      Tag         := StrToInt(Format('%d%d',[row,col]));
      Parent      := PanelLCDCreate;
      BorderStyle := bsSingle;
      BorderWidth := 0;
      ShowHint    := False;
      OnClick     := @PanelLCDEditClick;
      OnEnter     := @PanelLCDEditEnter;
      OnExit      := @PanelLCDEditExit;
      OnChange    := @PanelLCDEditChange;
      OnKeyDown   := @PanelLCDEditKeyDown;
   end;
  end;

begin
 FormMain.BeginFormUpdate;
 // Create LABELS row & col: row, col, left, top, caption
 ColNr := 1;
 for RowNr := 1 to FLCDSelected.Rows do begin
   CreateMatrixLabel(RowNr, ColNr,
     0 + (ColNr - 1) * (LCD_CHARWIDTH + 10),
     20 + (RowNr - 1) * (LCD_CHARHEIGHT +  10),
     IntToStr(RowNr));
 end;
 RowNr := 1;
 for ColNr := 1 to FLCDSelected.Cols do begin
   CreateMatrixLabel(RowNr, ColNr,
     20 + (ColNr - 1) * (LCD_CHARWIDTH + 10),
     0 + (RowNr - 1) * (LCD_CHARHEIGHT +  10),
     IntToStr(ColNr));
 end;
 // Create EDIT fields
 for RowNr := 1 to FLCDSelected.Rows do begin
    for ColNr := 1 to FLCDSelected.Cols do begin
      // row, col, left, top
      CreateCharEdit(RowNr, ColNr,
        20 + (ColNr - 1) * (LCD_CHARWIDTH + 10),
        30 + (RowNr - 1) * (LCD_CHARHEIGHT +  10) );
    end;
 end;
 // Init the chars
 FLCDSelected.Chars := InitLCDChars(LCD_ROWS, LCD_COLS);
 FormMain.EndFormUpdate;
end;

(*
  Move the cursor within the LCD Edit Fields
*)
procedure TFormMain.PanelLCDEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
 if (ssCtrl in Shift) then begin end;
 if (Key = VK_DOWN) then begin
   if FRow < FLCDSelected.Rows then Inc(FRow);
 end;
 if (Key = VK_UP) then begin
   if FRow > 1 then Dec(FRow);
 end;
 if (Key = VK_RIGHT) then begin
   if FCol < FLCDSelected.Cols then Inc(FCol);
 end;
 if (Key = VK_LEFT) then begin
   if FCol > 1 then Dec(FCol);
 end;
 SetLCDEditFocus(FRow, FCol);
end;

procedure TFormMain.PanelLCDEditClick(Sender: TObject);
begin
 // showmessage('PanelCharMatrixClick');
end;

(*
  Handle entering an edit field.
  Set the lcdeditselected and get the edit field pos FRow & FCol.
  Show the selected char in the table char list incl. char 5x8 pixel preview.
*)
procedure TFormMain.PanelLCDEditEnter(Sender: TObject);
begin
 FLCDEditSelected := Sender As TEdit;
 GetLCDEditPos(FLCDEditSelected, FRow, FCol);
 SetLCDTableCharsIndex(Sender, FLCDSelected.Chars[FRow,FCol].Char.CharIndex);
 SetStatusBarChar();
end;

(*
  Handle changes of the char edit fields.
  If edit field is empty, remove the char, i.e. set to default.
  If the char type is normal, then set the ascii char value as integer.
  If the char type is custom or table char, the changes are handled by the insertspecialchar function.
*)
procedure TFormMain.PanelLCDEditChange(Sender: TObject);
begin
 // if loading a LCD definition file then leave
 if FLoading then Exit;
 FLCDEditSelected := Sender As TEdit;
 // Empty field, remove char and leave
 if Length(FLCDEditSelected.Text) = 0 then begin
  ActionEditRemoveCharExecute(Sender);
  Exit;
 end;
 // Assign the edit text to the lcd array
 with FLCDSelected.Chars[FRow,FCol].Char do begin
    CharName := FLCDEditSelected.Text;
    if CharType = TypeNormal then begin
      CharIndex := Ord(CharName[1]);
      CharPattern := FLCDTableChars[CharIndex].CharPattern;
    end;
    SetLCDTableCharsIndex(Sender, CharIndex);
 end;
 DrawImageLCDPreview(ImageLCDPreview);
 SetStatusBarChar();
end;

procedure TFormMain.PanelLCDEditExit(Sender: TObject);
begin
  // FLCDSelected.Chars[FRow,FCol].Char := FLCDEditSelected.Text;
end;

(*
  Load the LCD display definition file in JSON format.
  See SaveLCDJSON.
*)
Procedure TFormMain.LoadLCDJSON(Sender: TObject;AFileName: String);
var
  sl: TStringList;
  jsonStr: String;
  jsonData: TJSONData;
  i, Row, Col: Integer;
  id: String;
begin
 FLoading := True;
 try
   //
    sl := TStringList.Create;
    sl.LoadFromFile(AFileName);
    jsonStr := sl.Text;
    sl.Free;
    jsonData := GetJSON(jsonStr);
    ComboBoxLCDSelected.ItemIndex := jsonData.FindPath('lcdtype').AsInteger;
    FLCDSelected := FLCDDisplays[ComboBoxLCDSelected.ItemIndex];
    SetLCDFormat(Sender);
    // Load the custom chars with index 0-7 into listbox and array FLCDCustomChars
    // name,type,index,value, i.e. Battery:C:0:14,27,17,17,17,17,17,31
    for i:=0 to jsonData.FindPath('customchars').AsInteger - 1 do begin
      id := Format('%d',[i]);
      with FLCDTableChars[i] do begin
        CharName  := jsonData.FindPath(Format('%s.name',[id])).AsString;
        CharType  := TypeCustom;
        CharIndex := jsonData.FindPath(Format('%s.index',[id])).AsInteger;
        CharPattern := jsonData.FindPath(Format('%s.pattern',[id])).AsString;
        StringGridLCDTableChars.Cells[0, i] := CharName;
      end;
    end;
    // Load the characters
    for Row := 1 to FLCDSelected.Rows do begin
      for Col := 1 to FLCDSelected.Cols do begin
        id := Format('%d%d',[Row,Col]);
        { TODO 3 -cFile : Check if properties Row&Col are required }
        FLCDSelected.Chars[Row,Col].Row := Row;
        FLCDSelected.Chars[Row,Col].Col := Col;
        with FLCDSelected.Chars[Row,Col].Char do begin
          CharName  := jsonData.FindPath(Format('%s.name',[id])).AsString;
          CharIndex := jsonData.FindPath(Format('%s.index',[id])).AsInteger;
          CharPattern := jsonData.FindPath(Format('%s.pattern',[id])).AsString;
          CharType  := GetCharType(CharIndex);
          // Set the edit char field. The hint property is not used.
          SetLCDEditChar(Row, Col, CharType, CharName, '');
        end;
      end;
    end;
    except on E: Exception do
     ShowError(
      'Load LCD Display Definition File (JSON)',
      E.Message + LineEnding + id);
  end;
  FLoading := False;
end;

(*
  Save the LCD matrix a lcd display definition file in JSON format.
  Special characters are escaped.
  Each char has an unique ID composed out of rowcol. Example: row 1 col 3 = "13".
  The id is used to parse the char definition.
  The examples show special chars and custom char.
  "11":{"row":1,"col":1,"char":"\"","type":"N","value":"","index":-1},
  "12":{"row":1,"col":2,"char":"\\","type":"N","value":"","index":-1},
  "13":{"row":1,"col":3,"char":"C","type":"C","value":"14,27,17,17,31,31,31,31","index":1},
  In addition the custom chars are also saved to allow defining custom chars per display:
  "customchars":8,
  "0":{"name":"Battery","type":"C","index":0,"value":"14,27,17,17,17,17,17,31"},
  "1":{"name":"BatteryHalf","type":"C","index":1,"value":"14,27,17,17,31,31,31,31"},
  "2":{"name":"BatteryFull","type":"C","index":2,"value":"14,31,31,31,31,31,31,31"},
  "3":{"name":"ArrowDown","type":"C","index":3,"value":"0,4,4,4,31,14,4,0"},
  "4":{"name":"Pi","type":"C","index":4,"value":"0,0,31,10,10,10,19,0"},
  "5":{"name":"Omega","type":"C","index":5,"value":"0,14,17,17,17,10,27,0"},
  "6":{"name":"Section","type":"C","index":6,"value":"6,9,12,10,10,6,18,12"},
  "7":{"name":"Drop","type":"C","index":7,"value":"4,4,10,10,17,17,17,14"},
*)
Procedure TFormMain.SaveLCDJSON(Sender: TObject;AFileName: String);
var
  sl:TStringList;
  item: String;
  i, Row, Col: Integer;
  curid, lastid: String;
begin
  try
    sl := TStringList.Create;
    sl.Add('{');
    sl.Add('"lcdtype":' + IntToStr(ComboBoxLCDSelected.ItemIndex) + ',');
    sl.Add('"rows":' + IntToStr(FLCDSelected.Rows) + ',');
    sl.Add('"cols":' + IntToStr(FLCDSelected.Cols) + ',');
    // Add custom chars with index 0-7
    // "":{"index":0,"name":"Battery","pattern":"14,27,17,17,17,17,17,31"},
    sl.Add('"customchars":' + IntToStr(LCD_CHAR_CUSTOM_INDEX_MAX) + ',');
    for i:=LCD_CHAR_CUSTOM_INDEX_MIN to LCD_CHAR_CUSTOM_INDEX_MAX do begin
      with FLCDTableChars[i] do begin
        item :=
          '"' + Format('%d',[i]) + '":{' +
          '"index":' + IntToStr(CharIndex)  + ',' +
          '"name":"' + CharName + '",' +
          '"pattern":"' + CharPattern + '"},';
        end;
      sl.Add(item);
    end;
    // Set the last ID used to check if , is added to an entry
    lastid := Format('%d%d', [FLCDSelected.Rows,FLCDSelected.Cols]);
    for Row := 1 to FLCDSelected.Rows do begin
      for Col := 1 to FLCDSelected.Cols do begin
        curid := Format('%d%d', [Row,Col]);
        With FLCDSelected.Chars[Row,Col].Char do begin
          // Escape special chars, like " to \" or \ to \\
          item := '"' + Format('%d%d',[Row,Col]) + '":{' +
            '"row":' + IntToStr(Row) + ',' +
            '"col":' + IntToStr(Col) + ',' +
            '"name":"' + StringTojsonString(CharName) + '",' +
            '"index":' + IntToStr(CharIndex)  + ',' +
            '"pattern":"' + CharPattern +'"}';
          if (curid <> lastid) then item := item + ',';
          sl.Add(item);
        end;
      end;
    end;
    sl.Add('}');
    sl.SaveToFile(AFileName);
    sl.Free;
  except
    on E: Exception do
     ShowError(
      'Save LCD Display Definition File (JSON)',
      E.Message + LineEnding + curid);
  end;
end;

(*
  Clear the text & hint of the LCD
*)
Procedure TFormMain.ClearLCD(Sender: TObject);
var
 i : Integer;
 ParentControl: TWinControl;
 LCDEdit: TEdit;
begin
  ParentControl := PanelLCDCreate;
  for i := 0 to ParentControl.ControlCount - 1 do begin
    if ParentControl.Controls[i] is TEdit then begin
     LCDEdit := TEdit(ParentControl.Controls[i]);
     LCDEdit.Text := '';
     LCDEdit.Hint := '';
     LCDEdit.Font.Color := LCD_EDIT_NORMALCHAR_COLOR;
    end;
  end;
  FLCDSelected.Chars := InitLCDChars(LCD_ROWS, LCD_COLS);
  ClearImageLCDTableChar(ImageLCDTableCharPreview);
  DrawImageLCDPreview(ImageLCDPreview);
  SetLCDEditFocus(1,1);
end;

//
// GETTER & SETTER
//

(*
  NOT USED = Keep for future
  Get the char from a tedit field property tag.
  The tag holds a number rowcol, i.e. row 2, col 3 = 23
  Returns Char
  Example: ShowMessage(GetLCDEditChar(1,3)));   // L
*)
Function TFormMain.GetLCDEditChar(ARow, ACol: Integer): String;
var
 ParentControl: TWinControl;
 LCDEdit: TEdit;
 Row, Col: Integer;
 i : Integer;
begin
  Row := -1;
  Col := -1;
  GetLCDEditChar := '';
  ParentControl := PanelLCDCreate;
  for i := 0 to ParentControl.ControlCount - 1 do begin
    if ParentControl.Controls[i] is TEdit then begin
     LCDEdit := TEdit(ParentControl.Controls[i]);
     GetLCDEditPos(LCDEdit, Row, Col);
     if (Row = ARow) and (Col = ACol) then begin
        GetLCDEditChar := LCDEdit.Text;
        Break;
      end;
    end;
  end;
end;

(*
  Set a char in a edit field.
  ARow, ACol - Edit field position
  AChar - Character
  Example: SetLCDEditChar(1,3,'L','');
*)
Procedure TFormMain.SetLCDEditChar(ARow, ACol: Integer; ACharType: TLCDCharTypes; AChar, AHint: String);
var
 ParentControl: TWinControl;
 LCDEdit: TEdit;
 Row, Col: Integer;
 i : Integer;
begin
 Row := -1;
 Col := -1;
  ParentControl := PanelLCDCreate;
  for i := 0 to ParentControl.ControlCount - 1 do begin
    if ParentControl.Controls[i] is TEdit then begin
      LCDEdit := TEdit(ParentControl.Controls[i]);
      GetLCDEditPos(LCDEdit, Row, Col);
      if (Row = ARow) and (Col = ACol) then begin
        LCDEdit.Text := AChar;
        LCDEdit.Hint := AHint;
        if ACharType = TypeNormal then LCDEdit.Font.Color := LCD_EDIT_NORMALCHAR_COLOR;
        if ACharType = TypeCustom then LCDEdit.Font.Color := LCD_EDIT_CUSTOMCHAR_COLOR;
        if ACharType = TypeExtended then LCDEdit.Font.Color := LCD_EDIT_EXTENDEDCHAR_COLOR;
        Break;
      end;
    end;
  end;
end;

(*
  Get the LCD position of an edit field from its tag property.
  The tag property has value rowcol, i.e. 416 for row=4, col=16.
  Returns: Row, Col
  Example: GetLCDEditPos(Sender As TEdit, Row, Col)
*)
Procedure TFormMain.GetLCDEditPos(AEdit: TEdit; Var ARow, ACol: Integer);
var
  TagChar: Integer;
  TagCharStr: String;
begin
  TagChar  := AEdit.Tag;
  TagCharStr := IntToStr(TagChar);
  ARow := StrToInt(TagCharStr.Substring(0,1));
  ACol := StrToInt(TagCharStr.Substring(1,Length(TagCharStr)));
end;

(*
  NOT USED = Keep for future
  Get the LCD edit field from its tag property.
  The tag property has value rowcol, i.e. 416 for row=4, col=16.
  Returns: Row, Col
  Example: GetLCDEditPos(Sender As TEdit, Row, Col)
*)
Function TFormMain.GetLCDEdit(ARow, ACol: Integer):TEdit;
var
 ParentControl: TWinControl;
 LCDEdit: TEdit;
 Row, Col: Integer;
 i : Integer;
begin
 Row := -1;
 Col := -1;
  ParentControl := PanelLCDCreate;
  for i := 0 to ParentControl.ControlCount - 1 do begin
    if ParentControl.Controls[i] is TEdit then begin
      LCDEdit := TEdit(ParentControl.Controls[i]);
      GetLCDEditPos(LCDEdit, Row, Col);
      if (Row = ARow) and (Col = ACol) then begin
        GetLCDEDit := LCDEdit;
        Break;
      end;
    end;
  end;
end;

(*
  Set focus on a char edit field by setting row col position
  Example: SetLCDEditFocus(1,3);
*)
Procedure TFormMain.SetLCDEditFocus(ARow, ACol: Integer);
var
 ParentControl: TWinControl;
 LCDEdit: TEdit;
 Row, Col: Integer;
 i : Integer;
begin
 Row := -1;
 Col := -1;
 ParentControl := PanelLCDCreate;
 for i := 0 to ParentControl.ControlCount - 1 do begin
   if ParentControl.Controls[i] is TEdit then begin
     LCDEdit := TEdit(ParentControl.Controls[i]);
     GetLCDEditPos(LCDEdit, Row, Col);
     if (Row = ARow) and (Col = ACol) then begin
       LCDEdit.SetFocus;
       SetStatusBarChar();
       Break;
     end;
   end;
 end;
end;

(*
  Get the position of a field like TLabel, TEdit from its tag property.
  The tag property has value rowcol, i.e. 416 for row=4, col=16.
  This procedure is mainly used for setting the LCD display format, i.e. 1602, 2004 etc.
  Returns: Row, Col
  Example: GetLCDControlPos(Sender As TEdit, Row, Col)
*)
Procedure TFormMain.GetLCDControlPos(AControl: TControl; Var ARow, ACol: Integer);
var
  TagChar: Integer;
  TagCharStr: String;
begin
  TagChar  := AControl.Tag;
  TagCharStr := IntToStr(TagChar);
  ARow := StrToInt(TagCharStr.Substring(0,1));
  ACol := StrToInt(TagCharStr.Substring(1,Length(TagCharStr)));
end;

//
// LCDDisplay Matrix = 2 dim array rows & cols
//

(*
  Init LCD Display Properties.
  Example: FLCDDisplays[1] := InitLCDDisplay('LCD2004',1,'0x27',4, 20);
*)
Function TFormMain.InitLCDDisplay(AName:String;AIndex:Integer;AAddress:String;ARows,ACols:Integer):TLCDDisplay;
begin
 with InitLCDDisplay do begin
   Name     := AName;
   Index    := AIndex;
   Address  := AAddress;
   Cols     := ACols;
   Rows     := ARows;
   Chars    := InitLCDChars(LCD_ROWS, LCD_COLS);
 end;
end;

(*
  Init the LCD Display Chars, i.e. 2 dim matrix rows & cols.
  Init the Chars 2 dim array with highest LCD size, i.e. 4 rows & 20 cols
  Set char type to normal.
  Example: FLCDSelected := InitLCDChars(LCD_ROWS, LCD_COLS);
*)
Function TFormMain.InitLCDChars(ARows,ACols: Integer):TLCDChars;
var
  RowNr,ColNr: Integer;
begin
   for RowNr := 1 to ARows do
     for ColNr := 1 to ACols do begin
       with InitLCDChars[RowNr,ColNr] do begin
         Row := RowNr;
         Col := ColNr;
         with Char do begin
           CharName    := '';
           CharType    := TypeNormal;
           CharPattern := LCD_CHAR_PATTERN_EMPTY;
           CharIndex   := -1;
         end;
       end;
     end;
end;

(*
  Get a char from the LCD display matrix
*)
Function TFormMain.GetLCDChar(ALCDDisplay:TLCDDisplay;ARow,ACol:Integer):TLCDChar;
begin
 GetLCDChar := ALCDDisplay.Chars[ARow,ACol];
end;

//
// TABLECHARS
//

(*
  Set the LCD table char item index based on the char index.
  A listbox item shows name:charindex.
  Need to get the charindex from the listbox item.
  { TODO 2 -cChar : Improve search routine - use for each or indexof with charname:charindex. }
*)
procedure TFormMain.SetLCDTableCharsIndex(Sender:TObject; AIndex: Integer);
var
  i:Integer;
begin
 if AIndex < 0 then Exit;
 with StringGridLCDTableChars do begin
   if RowCount = 0 then exit;
   // Loop over the stringgrid starting at row 1 because row 0 is titlerow
   for i := 1 to RowCount - 1 do begin
     if StrToInt(Cells[1,i]) = AIndex then begin
        Row := i;
        Exit;
     end;
   end;
 end;
end;

function TFormMain.GetLCDTableCharsIndex(Sender:TObject):Integer;
begin
 GetLCDTableCharsIndex := StringGridLCDTableChars.Row;
end;

(*
  Check if the char in the edit field is a custom char, i.e. index <= 7.
*)
Function TFormMain.IsLCDCustomChar(ALCDDisplay:TLCDDisplay;ARow,ACol:Integer):Boolean;
begin
 IsLCDCustomChar := ALCDDisplay.Chars[ARow,ACol].Char.CharIndex <= LCD_CHAR_CUSTOM_INDEX_MAX;
end;

(*
  Get a table char record from its definition index:name:pattern.
  At formcreate, the chars definitions are loaded from an external file.
  Split the char properties into 3 items: index(0):name(1):pattern(2)
  which are assigned to CharIndex,CharName,CharPattern
  223:Degree:28,20,28,0,0,0,0,0
  1:BatteryHalf:14,27,17,17,31,31,31,31
  Battery := GetLCDTableCharFromDefinition(ListBoxCustomChars.Items[1]);
*)
Function TFormMain.GetLCDTableCharFromDefinition(ACharDefinition: String):TLCDTableChar;
var
  Items: TStringArray;
begin
 Items := ACharDefinition.Split(LCD_DELIMITER);
 with GetLCDTableCharFromDefinition do begin
   CharIndex  := StrToInt(Items[0]);
   CharName   := Items[1];
   CharPattern  := Items[2];
 end;
end;

(*
  Get table char record from its index.
*)
Function TFormMain.GetLCDTableCharFromIndex(AIndex: Integer):TLCDTableChar;
begin
 GetLCDTableCharFromIndex := FLCDTableChars[AIndex];
end;

(*
  Get the name of a table char by its unique index
  Example:
  GetLCDTableCharName(223) returns Degree.
*)
Function TFormMain.GetLCDTableCharName(AIndex: Integer):String;
var
  i:Integer;
begin
 GetLCDTableCharName := 'UNKNOWN';
 for i:=0 to Length(FLCDTableChars) - 1 do begin
   if FLCDTableChars[i].CharIndex = AIndex then begin
     GetLCDTableCharName := FLCDTableChars[i].CharName;
     Break;
   end;
 end;
end;

(*
  Get the type of char defined by its table char index.
  CharTypes with table index: N)ormal 32-127,C)ustom 0-7,X)xtended 128-255
*)
Function TFormMain.GetCharType(AIndex:Integer):TLCDCharTypes;
begin
 GetCharType := TypeNormal;
 if ((AIndex >= LCD_CHAR_CUSTOM_INDEX_MIN) And (AIndex <= LCD_CHAR_CUSTOM_INDEX_MAX)) then
   GetCharType := TypeCustom;
 if ((AIndex >= LCD_CHAR_EXT_INDEX_MIN) And (AIndex <= LCD_CHAR_EXT_INDEX_MAX)) then
   GetCharType := TypeExtended;
end;

(*
  Set the char type as string from the chartype.
*)
Function TFormMain.SetCharTypeStr(ACharType: TLCDCharTypes): String;
begin
  case ACharType of
    TypeNormal: SetCharTypeStr := LCD_EDIT_NORMALCHAR;
    TypeCustom: SetCharTypeStr := LCD_EDIT_CUSTOMCHAR;
    TypeExtended: SetCharTypeStr := LCD_EDIT_EXTENDEDCHAR;
  end;
end;

//
// IMAGES
//

(*
  Draw the lcd table char image.
*)
procedure TFormMain.DrawImageLCDTableChar(AImage: TImage; APattern: String);
var
  row,col: Integer;
  x1,y1,x2,y2: Integer;
  CharPattern: TStringArray;
  Pixels: String;
  CharGrid: Array[1..LCD_CHAR_ROWS,1..LCD_CHAR_COLS] of Integer;
  R: TRect;
begin
 AImage.Canvas.Clear;
 AImage.Canvas.Brush.Color := clWhite;
 AImage.Canvas.Rectangle(0, 0, AImage.ClientWidth, AImage.ClientHeight);
 // Check if there is a char pattern, else set empty pattern
 if Length(APattern.Trim) = 0 then
   APattern := LCD_CHAR_PATTERN_EMPTY;
 // Split the char pattern (int values) into a stringarray with 8 rows
 CharPattern := APattern.Split(',');
 // Create the chargrid
 for row := 0 to LCD_CHAR_ROWS - 1 do begin
   // Convert the row dec value to bin with 5 bits (pixels)
   Pixels := Dec2Numb(StrToInt(CharPattern[row]), LCD_FORMAT_BIN_BITS, 2);
   for col := 1 to LCD_CHAR_COLS do begin
    CharGrid[row + 1,col] := StrToInt(Pixels[col]);
   end;
 end;
 with AImage.Canvas do begin
   for row := 1 to LCD_CHAR_ROWS do begin
     for col:=1 to LCD_CHAR_COLS do begin
       x1 := col * LCD_CHAR_IMAGE_SIZE;
       y1 := row * LCD_CHAR_IMAGE_SIZE;
       x2 := x1 + LCD_CHAR_IMAGE_SIZE;
       y2 := y1 + LCD_CHAR_IMAGE_SIZE;
       R.Left := x1;
       R.Top := y1;
       R.Right := x2;
       R.Bottom := y2;
       Pen.Width := 1;
       Pen.Color := TColor($DCDCDC);
       if CharGrid[row,col] = 0 then
         Brush.Color := clWhite
       else
         Brush.Color := clRed;
       Rectangle(R);
     end;
   end;
 end;
end;

(*
  Clear the lcd table char image.
*)
procedure TFormMain.ClearImageLCDTableChar(AImage: TImage);
begin
 AImage.Canvas.Clear;
 AImage.Canvas.Pen.Width := 1;
 AImage.Canvas.Pen.Color := TColor($DCDCDC);
 AImage.Canvas.Brush.Color := clWhite;
 AImage.Canvas.Rectangle(0, 0, AImage.ClientWidth, AImage.ClientHeight);
end;

//LCDPREVIEW

(*
  Clear the LCD preview
*)
procedure TFormMain.ClearImageLCDPreview(AImage: TImage);
begin
 with AImage do begin
  Canvas.Clear;
  Canvas.Brush.Color := clDefault;                   //clWhite;
  Canvas.Pen.Width := 5;
  Canvas.Pen.Color := clBlack;
  Canvas.Rectangle(0, 0, ClientWidth, ClientHeight);
 end;
end;

(*
  Draw the LCD Preview depending LCD size.
  Set the color of the LCD preview according to a scheme.
  The color of a pixel is set by its state 0 or 1.
  Example color scheme green is background green (state 0) and forground is black (state 1).
*)
procedure TFormMain.DrawImageLCDPreview(AImage: TImage);
const
  BORDERSPACE = 5;
var
  row,col: Integer;
  LCDChar: TLCDTableChar;

  procedure DrawLCDChar(AChar:TLCDTableChar;AXPos,AYPos:Integer);
  var
    row,col: Integer;
    x1,y1,x2,y2: Integer;
    CharPattern: TStringArray;
    Pixels: String;
    CharGrid: Array[1..LCD_CHAR_ROWS,1..LCD_CHAR_COLS] of Integer;
    R: TRect;
  begin
   // Check if there is text and a char pattern
   // if Length(AChar.CharName.Trim) = 0 then Exit;
   // if Length(AChar.CharPattern.Trim) = 0 then Exit;
   if Length(AChar.CharPattern.Trim) = 0 then
     AChar.CharPattern := LCD_CHAR_PATTERN_EMPTY;
   CharPattern := AChar.CharPattern.Split(',');
   for row := 0 to LCD_CHAR_ROWS - 1 do begin
     Pixels := Dec2Numb(StrToInt(CharPattern[row]), LCD_FORMAT_BIN_BITS, 2);
     for col := 1 to LCD_CHAR_COLS do begin
      CharGrid[row + 1,col] := StrToInt(Pixels[col]);
     end;
   end;
   with AImage.Canvas do begin
     for row := 1 to LCD_CHAR_ROWS do begin
       for col:=1 to LCD_CHAR_COLS do begin
         x1 := AXPos + (col * LCD_CHAR_IMAGE_SIZE);
         y1 := AYPos + (row * LCD_CHAR_IMAGE_SIZE);
         x2 := x1 + LCD_CHAR_IMAGE_SIZE;
         y2 := y1 + LCD_CHAR_IMAGE_SIZE;
         R.Left := x1;
         R.Top := y1;
         R.Right := x2;
         R.Bottom := y2;
         Pen.Width := 1;
         Pen.Color := TColor($DCDCDC);
         if CharGrid[row,col] = 0 then
           Brush.Color := COLORSCHEMES[FLCDColorScheme].PixelLow
         else
           Brush.Color := COLORSCHEMES[FLCDColorScheme].PixelHigh;
         Rectangle(R);
       end;
     end;
   end;
  end;

begin
 with AImage do begin
   // Set the width of the LCD Preview: LCDSelected.Cols * CHARSIZE*CHARCOLS
   Width := (FLCDSelected.Cols * LCD_CHAR_IMAGE_SIZE * LCD_CHAR_COLS) + 30 + (2*BORDERSPACE);
   Height := (FLCDSelected.Rows * LCD_CHAR_IMAGE_SIZE * LCD_CHAR_RowS) + 20 + (2*BORDERSPACE);
   Canvas.Clear;
   Canvas.Brush.Color := COLORSCHEMES[FLCDColorScheme].Border;  // TColor($7FFF00); //TColor($DCDCDC);             // clWhite;
   Canvas.Pen.Width := LCD_PREVIEW_BORDER_WIDTH;
   Canvas.Pen.Color := clBlack;
   Canvas.Rectangle(0, 0, ClientWidth, ClientHeight);
 end;
 for row := 1 to FLCDSelected.Rows do begin
   for col := 1 to FLCDSelected.Cols do begin
    LCDChar := FLCDSelected.Chars[row,col].Char;
    DrawLCDChar(
      LCDChar,
      BORDERSPACE+(col-1)*(1 + LCD_CHAR_IMAGE_SIZE*LCD_CHAR_COLS),
      BORDERSPACE+(row-1)*(1 + LCD_CHAR_IMAGE_SIZE*LCD_CHAR_ROWS));
   end;
 end;
end;

(*
  Create the list of LCD preview color schemes from the constant COLORSCHEMES.
  Set index 1 as default and draw the LCD preview.
*)
Procedure TFormMain.SetLCDPreviewColorSchemeSelection(Sender: TObject);
var
  i: Integer;
begin
 for i:=1 to Length(COLORSCHEMES) do
   ListBoxLCDPreviewColorScheme.Items.Add(COLORSCHEMES[i].Name);
 ListBoxLCDPreviewColorScheme.ItemIndex := 1;
 ListBoxLCDPreviewColorSchemeClick(Sender);
end;

//
// HELPERS
//
(*
  Set statusbar simpletext
*)
Procedure TFormMain.SetStatusBar(ARow, ACol: Integer; AChar, AHint: String);
begin
  StatusBarMain.SimpleText := Format(
    'Row: %d Col: %d Char: %s Hint: %s',
    [ARow, ACol, AChar, AHint]);
end;

(*
  Set statusbar char information for the selected char based on pos row,col
*)
Procedure TFormMain.SetStatusBarChar;
var
  s: String;
begin
 with FLCDSelected.Chars[FRow,FCol].Char do begin
   s := Format(
    'Row:%d, Col:%d, Index:%d, Name:%s, Type:%s, Pattern:%s',
    [FRow,FCol,CharIndex,CharName,SetCharTypeStr(CharType),CharPattern]);
   StatusBarMain.SimpleText := s;
  end;
end;

end.

