unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  DbCtrls, UMetadata, sqldb, UListView, UAbout;

type

  { TMainForm }

  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    MenuItemAbout: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemReference: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure AddReferenceItem(AName, ACaption: String; ATag: Integer);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;



var
  MainForm: TMainForm;

implementation

function IsFormOpen(AName: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 To Screen.FormCount - 1 do
    if (Screen.Forms[i].Name = AName) then
    begin
      Result := True;
      Screen.Forms[i].BringToFront;
      Break;
    end;
end;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to High(Tables) do
    AddReferenceItem(Tables[i].Name, Tables[i].Caption, i);
end;

procedure TMainForm.AddReferenceItem(AName, ACaption: String; ATag: Integer);
var
  AReferenceItem: TMenuItem;
begin
  AReferenceItem := TMenuItem.Create(MainMenu);
  AReferenceItem.Caption := ACaption;
  AReferenceItem.Name := AName;
  AReferenceItem.Tag := ATag;
  AReferenceItem.OnClick := @MenuItemClick;
  MenuItemReference.Add(AReferenceItem);
end;

procedure TMainForm.MenuItemAboutClick(Sender: TObject);
begin
  FormAbout.Show;
end;

procedure TMainForm.MenuItemClick(Sender: TObject);
begin
  if not(IsFormOpen((Sender as TMenuItem).Name)) then
    FormListView.CreateNew((Sender as TMenuItem).Name, (Sender as TMenuItem).Caption, (Sender as TMenuItem).Tag);
end;

procedure TMainForm.MenuItemExitClick(Sender: TObject);
begin
    if MessageDlg('Выйти из программы?', mtConfirmation, mbYesNo, 0) = mrYes then
      MainForm.Close;
end;


end.
