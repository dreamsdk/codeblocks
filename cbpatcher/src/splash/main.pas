unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    pnlSplash: TPanel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
  public
  end;

var
  frmMain: TfrmMain;

implementation

uses
  SysTools;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Delay(1500);
  Application.Terminate;
end;

end.

