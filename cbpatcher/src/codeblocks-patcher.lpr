program CodeBlocksPatcher;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, CBPatch, Version;

type
  { TCodeBlocksPatcherApplication }
  TCodeBlocksPatcherApplication = class(TCustomApplication)
  private
    fCodeBlocksPatcher: TCodeBlocksPatcher;
    procedure WriteHeader;
  protected
    procedure DoInitialize;
    procedure DoRun; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    property Patcher: TCodeBlocksPatcher
      read fCodeBlocksPatcher;
  end;

{ TCodeBlocksConfigurationInjector }

procedure TCodeBlocksPatcherApplication.WriteHeader;
var
  ReleaseType: string;

begin
  ReleaseType := EmptyStr;
  if IsDebugBuild then
    ReleaseType := '-DEBUG';

  WriteLn(Title, ', Ver. ', GetFileVersion, ReleaseType);
  WriteLn('Created by ', GetCompanyName, '.', sLineBreak);
end;

procedure TCodeBlocksPatcherApplication.DoRun;
const
  ERR_HELP_PASSED = 1;
  ERR_INVALID_OPTIONS = 2;

var
  ErrorMsg: string;

begin
  WriteHeader;

  // quick check parameters
  ErrorMsg := CheckOptions('h', 'help');
  if ErrorMsg <> EmptyStr then
  begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then
  begin
    WriteHelp;
    Terminate(ERR_HELP_PASSED);
    Exit;
  end;

  // initialize the patcher engine
  DoInitialize;

  // execute it!
  with Patcher do
    if Ready then
    begin
      InstallPatch;
      ApplyConfiguration;
    end
    else
    begin
      WriteLn('Error: Missing or invalid parameters.', sLineBreak);
      WriteHelp;
      Terminate(ERR_INVALID_OPTIONS);
      Exit;
    end;

  // stop program loop
  Terminate;
end;

procedure TCodeBlocksPatcherApplication.DoInitialize;
begin
  if ParamCount = 4 then
    with Patcher do
    begin
      SourceDirectory := ParamStr(1);
      SoftwareDevelopmentKitHomeDirectory := ParamStr(2);
      CodeBlocksConfigurationFileName := ParamStr(3);
      CodeBlocksInstallationDirectory := ParamStr(4);
    end;
end;

constructor TCodeBlocksPatcherApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  StopOnException := True;
  fCodeBlocksPatcher := TCodeBlocksPatcher.Create;
end;

destructor TCodeBlocksPatcherApplication.Destroy;
begin
  fCodeBlocksPatcher.Free;
  inherited Destroy;
end;

procedure TCodeBlocksPatcherApplication.WriteHelp;
var
  CmdName: string;

begin
  CmdName := ExtractFileName(ChangeFileExt(ExeName, EmptyStr));
  WriteLn('Usage:', sLineBreak,
    '  ', CmdName, ' <SOURCE_DIR> <DREAMSDK_DIR> <CB_CONF_FILE> <CB_INST_DIR>', sLineBreak,
    sLineBreak,
    'Description:', sLineBreak,
    '  SOURCE_DIR   : The directory where are stored the required files for the', sLineBreak,
    '                 patcher.', sLineBreak,
    '  DREAMSDK_DIR : The DreamSDK Home directory (typically ''C:\DreamSDK\'').', sLineBreak,
    '  CB_CONF_FILE : The Code::Blocks configuration file to patch (typically ', sLineBreak,
    '                 ''default.conf'').', sLineBreak,
    '  CB_INST_DIR  : The Code::Blocks installation directory (typically ', sLineBreak,
    '                 ''C:\Program Files\CodeBlocks\'').', sLineBreak,
    sLineBreak,
    'Note:', sLineBreak,
    '  This patcher was made for the Code::Blocks 17.12 stable release.',
    sLineBreak);
end;

var
  Application: TCodeBlocksPatcherApplication;

{$R *.res}

begin
  Application := TCodeBlocksPatcherApplication.Create(nil);
  try
    Application.Title:='Code::Blocks Patcher';
    Application.Run;
  finally
    Application.Free;
  end;
end.

