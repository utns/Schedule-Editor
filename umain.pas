unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  DbCtrls, UMetadata, sqldb, UListView, UAbout, UEditForm, UScheduleForm;

type

  { TMainForm }

  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    MenuItemSchedule: TMenuItem;
    MenuItemAbout: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemReference: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure AddReferenceItem(AName, ACaption: String; ATag: Integer);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure MenuItemScheduleClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation

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
  with (Sender as TMenuItem) do
    CreateNewListViewForm(Name, Caption, Tag, ftListView);
end;

procedure TMainForm.MenuItemExitClick(Sender: TObject);
begin
  if MessageDlg('Выйти из программы?', mtConfirmation, mbYesNo, 0) = mrYes then
    MainForm.Close;
end;

procedure TMainForm.MenuItemScheduleClick(Sender: TObject);
begin
  ScheduleForm.Show;
end;

procedure ActivateSQL;
var
  i: Integer;
begin
  for i := 0 to High(ListViewForms) do
    ListViewForms[i].OpenSQLQuery;
  ScheduleForm.FillDrawGrid;
  ScheduleForm.HideEmptyColRow;
  ScheduleForm.Invalidate;  ///
end;

initialization
  EActivateSQL := @ActivateSQL;

end.

