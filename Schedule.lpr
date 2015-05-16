program Schedule;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, umain, UDBConnection, UMetadata, UListView, UAbout, USQLQueries,
  UFilters, UEditForm, UEditors, UScheduleForm;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TDataModuleMain, DataModuleMain);
  Application.CreateForm(TFormListView, FormListView);
  Application.CreateForm(TFormAbout, FormAbout);
  Application.CreateForm(TEditForm, EditForm);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

