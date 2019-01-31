program CodeBlocksPatcher;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, CBPatch;

type
  { TCodeBlocksConfigurationInjector }
  TCodeBlocksConfigurationInjector = class(TCustomApplication)
  private
    fCodeBlocksConfigurationManager: TCodeBlocksConfigurationManager;
    procedure WriteHeader;
  protected
    procedure DoInitialize(const FragmentDirectory, TargetConfigurationFileName: TFileName);
    procedure DoRun; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    property ConfigurationManager: TCodeBlocksConfigurationManager
      read fCodeBlocksConfigurationManager;
  end;

{ TCodeBlocksConfigurationInjector }

procedure TCodeBlocksConfigurationInjector.WriteHeader;
begin
{$IFDEF DEBUG}
//  WriteLn('DEBUG');
{$ENDIF}
end;

procedure TCodeBlocksConfigurationInjector.DoRun;
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
    Terminate;
    Exit;
  end;

  { add your program here }
  DoInitialize('..\rsrc', 'work.conf');
  ConfigurationManager.Merge;

  // stop program loop
  Terminate;
end;

procedure TCodeBlocksConfigurationInjector.DoInitialize(const FragmentDirectory,
  TargetConfigurationFileName: TFileName);
begin
  ConfigurationManager.FragmentDirectory := FragmentDirectory;
  ConfigurationManager.TargetFileName := TargetConfigurationFileName;
  ConfigurationManager.BaseDirectory := 'E:\DreamSDK\';
end;

constructor TCodeBlocksConfigurationInjector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  StopOnException := True;
  fCodeBlocksConfigurationManager := TCodeBlocksConfigurationManager.Create;
end;

destructor TCodeBlocksConfigurationInjector.Destroy;
begin
  fCodeBlocksConfigurationManager.Free;
  inherited Destroy;
end;

procedure TCodeBlocksConfigurationInjector.WriteHelp;
begin
  { add your help code here }
  WriteLn('Usage: ', ExeName, ' -h');
end;

var
  Application: TCodeBlocksConfigurationInjector;

{$R *.res}

begin
  Application := TCodeBlocksConfigurationInjector.Create(nil);
  try
    Application.Title:='Code::Blocks Patcher';
    Application.Run;
  finally
    Application.Free;
  end;
end.

