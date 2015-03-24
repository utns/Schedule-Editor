unit UMetadata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TMyField }

  TMyField = class
  private
    FName, FCaption: String;
    FWidth: Integer;
    FVisible: Boolean;
  public
    Constructor Create(AName, ACaption: String; AWidth: Integer; AVisible: Boolean);
    property Name: String read FName;
    property Caption: String read FCaption;
    property Width: Integer read FWidth;
    property Visible: Boolean read FVisible;
  end;

  { TMyJoinedField }

  TMyJoinedField = class(TMyField)
  private
    FJoinedTable, FJoinedField, FVisField: String;
  public
    Constructor Create(AName, ACaption, AJoinedTable, AJoinedField, AVisField: String; AWidth: Integer);
    property JoinedTable: String read FJoinedTable;
    property JoinedField: String read FJoinedField;
    property VisField: String read FVisField;
  end;

  { TMyTable }

  TMyTable = class
  private
    FName, FCaption: String;
    FFields: array of TMyField;
    function GetField(Index: Integer): TMyField;
  public
    Constructor Create(AName, ACaption: String);
    function CreateSqlQuery: String;
    procedure AddNewField(AName, ACaption: String; AWidth: Integer; AVisible: Boolean);
    procedure AddNewField(AName, ACaption, AJoinedTable, AJoinedFieldName, AVisField: String;
      AWidth: Integer);
    function GetFieldsLength: Integer;
    property Name: String read FName;
    property Caption: String read FCaption;
    property Fields[Index: Integer]: TMyField read GetField;
  end;

  var
    Tables: array of TMyTable;

implementation

procedure RegisterTable(AName, ACaption: String);
var
  ATable: TMyTable;
begin
  SetLength(Tables, Length(Tables) + 1);
  ATable := TMyTable.Create(AName, ACaption);
  Tables[High(Tables)] := ATable;
end;

procedure RegisterTableField(ATableName, AName, ACaption: String;
  AWidth: Integer; AVisible: Boolean);
var
  i: Integer;
begin
  for i := 0 to High(Tables) do
    if Tables[i].Name = ATableName then
    begin
      Tables[i].AddNewField(AName, ACaption, AWidth, AVisible);
      Break;
    end;
end;

procedure RegisterTableField(ATableName, AName, ACaption, AJoinedTable, AJoinedFieldName, AVisField: String;
  AWidth: Integer);
var
  i: Integer;
begin
  for i := 0 to High(Tables) do
    if Tables[i].Name = ATableName then
    begin                                   //плохо
      Tables[i].AddNewField(AName, ACaption, AJoinedTable, AJoinedFieldName, AVisField, AWidth);
      Break;
    end;
end;

{ TMyJoinedField }

constructor TMyJoinedField.Create(AName, ACaption, AJoinedTable, AJoinedField,
  AVisField: String; AWidth: Integer);
begin
  inherited Create(AName, ACaption, AWidth, True);
  FJoinedTable := AJoinedTable;
  FJoinedField := AJoinedField;
  FVisField := AVisField;
end;

{ TMyField }

constructor TMyField.Create(AName, ACaption: String; AWidth: Integer;
  AVisible: Boolean);
begin
  Inherited Create;
  FName := AName;
  FCaption := ACaption;
  FWidth := AWidth;
  FVisible := AVisible;
end;

{ TMyTable }

function TMyTable.GetField(Index: Integer): TMyField;
begin
  Result := FFields[Index];
end;

constructor TMyTable.Create(AName, ACaption: String);
begin
  Inherited Create;
  FName := AName;
  FCaption := ACaption;
end;

function TMyTable.CreateSqlQuery: String;
var
  i: Integer;
begin
  if (Fields[0] is TMyJoinedField) then
    Result := 'Select ' + (Fields[0] as TMyJoinedField).JoinedTable
    + '.' + (Fields[0] as TMyJoinedField).VisField + ' AS ' + Fields[0].Name
  else
    Result := 'Select ' + Name + '.' + Fields[0].Name;

  for i := 1 to GetFieldsLength - 1 do
    if (Fields[i] is TMyJoinedField) then
      Result += ', ' + (Fields[i] as TMyJoinedField).JoinedTable
        + '.' + (Fields[i] as TMyJoinedField).VisField + ' AS ' + Fields[i].Name
    else
      Result += ', ' + Name + '.' + Fields[i].Name;

  Result += ' FROM ' + Name;

  for i := 0 to GetFieldsLength - 1 do
    if Fields[i] is TMyJoinedField then
      Result += ' INNER JOIN ' + (Fields[i] as TMyJoinedField).JoinedTable
        + ' ON ' + Name + '.' + Fields[i].Name
        + ' = ' + (Fields[i] as TMyJoinedField).JoinedTable + '.'
        + (Fields[i] as TMyJoinedField).JoinedField;
  Result += ' ORDER BY 1, 2';
end;

procedure TMyTable.AddNewField(AName, ACaption: String; AWidth: Integer;
  AVisible: Boolean);
begin
  SetLength(FFields, Length(FFields) + 1);
  FFields[High(FFields)] := TMyField.Create(AName, ACaption, AWidth, AVisible);
end;

procedure TMyTable.AddNewField(AName, ACaption, AJoinedTable, AJoinedFieldName,
  AVisField: String; AWidth: Integer);
begin
  SetLength(FFields, Length(FFields) + 1);
  FFields[High(FFields)] := TMyJoinedField.Create(AName, ACaption, AJoinedTable, AJoinedFieldName, AVisField, AWidth);
end;

function TMyTable.GetFieldsLength: Integer;
begin
  Result := Length(FFields);
end;


initialization
  RegisterTable('EducActivities', 'Учебная деятельность');
  RegisterTableField('EducActivities', 'EducID', 'ИД', 35, True);
  RegisterTableField('EducActivities', 'EducName', 'Название', 100, True);

  RegisterTable('Teachers', 'Преподаватели');
  RegisterTableField('Teachers', 'TeacherID', 'ИД', 35, True);
  RegisterTableField('Teachers', 'TeacherInitials', 'Ф.И.О. преподавателя', 250, True);

  RegisterTable('Groups', 'Группы');
  RegisterTableField('Groups', 'GroupID', 'ИД', 35, False);
  RegisterTableField('Groups', 'GroupNumber', 'Номер', 55, True);
  RegisterTableField('Groups', 'GroupName', 'Название группы', 285, True);

  RegisterTable('Students', 'Студенты');
  RegisterTableField('Students', 'StudentID', 'ИД', 35, True);
  RegisterTableField('Students', 'StudentInitials', 'Ф.И.О. студента', 250, True);
  RegisterTableField('Students', 'GroupID', 'Группа', 'Groups', 'GroupID', 'GroupNumber', 55);

  RegisterTable('Subjects', 'Предметы');
  RegisterTableField('Subjects', 'SubjectID', 'ИД', 35, True);
  RegisterTableField('Subjects', 'SubjectName', 'Название предмета', 250, True);

  RegisterTable('Audiences', 'Аудитории');
  RegisterTableField('Audiences', 'AudienceID', 'ИД', 35, True);
  RegisterTableField('Audiences', 'AudienceNumber', 'Номер', 150, True);

  RegisterTable('Pairs', 'Пары');
  RegisterTableField('Pairs', 'PairID', 'ИД', 35, True);
  RegisterTableField('Pairs', 'PairBegin', 'Начало', 55, True);
  RegisterTableField('Pairs', 'PairEnd', 'Конец', 50, True);
  RegisterTableField('Pairs', 'PairNumber', 'Номер', 45, True);

  RegisterTable('WeekDays', 'Дни недели');
  RegisterTableField('WeekDays', 'WeekDayID', 'ИД', 35, True);
  RegisterTableField('WeekDays', 'WeekDayName', 'Название', 90, True);
  {RegisterTableField('Students', 'StudentID', 'ИД', 35);
  RegisterTableField('Students', 'StudentInitials', 'Ф.И.О. студента', 250);
  RegisterTableField('Students', 'GroupID', 'Группа', 'Groups', 'GroupID', 'GroupNumber', 55);

  RegisterTable('Subjects', 'Предметы');
  RegisterTableField('Subjects', 'SubjectID', 'ИД', 35);
  RegisterTableField('Subjects', 'SubjectName', 'Название предмета', 250);

  RegisterTable('Audiences', 'Аудитории');
  RegisterTableField('Audiences', 'AudienceID', 'ИД', 35);
  RegisterTableField('Audiences', 'AudienceNumber', 'Номер', 150);

  RegisterTable('Pairs', 'Пары');
  RegisterTableField('Pairs', 'PairID', 'ИД', 35);
  RegisterTableField('Pairs', 'PairBegin', 'Начало', 55);
  RegisterTableField('Pairs', 'PairEnd', 'Конец', 50);
  RegisterTableField('Pairs', 'PairNumber', 'Номер', 45);

  RegisterTable('WeekDays', 'Дни недели');
  RegisterTableField('WeekDays', 'WeekDayID', 'ИД', 35);
  RegisterTableField('WeekDays', 'WeekDayName', 'Название', 90);
  RegisterTableField('WeekDays', 'WeekDayNumber', 'Номер', 45);}

  RegisterTable('Schedules', 'Расписание');
  RegisterTableField('Schedules', 'GroupID', 'Группа', 'Groups', 'GroupID', 'GroupNumber', 55);
  RegisterTableField('Schedules', 'WeekDayID', 'День недели', 'WeekDays', 'WeekDayID', 'WeekDayName', 90);
  RegisterTableField('Schedules', 'PairID', '№ пары', 'Pairs', 'PairID', 'PairNumber', 55);
  RegisterTableField('Schedules', 'SubjectID', 'Предмет', 'Subjects', 'SubjectID', 'SubjectName', 190);
  RegisterTableField('Schedules', 'EducID', 'Форма занятия', 'EducActivities', 'EducID', 'EducName', 100);
  RegisterTableField('Schedules', 'TeacherID', 'Преподаватель', 'Teachers', 'TeacherID', 'TeacherInitials', 250);
  RegisterTableField('Schedules', 'AudienceID', 'Аудитория', 'Audiences', 'AudienceID', 'AudienceNumber', 70);

  RegisterTable('Teachers_Subjects', 'Предметы преподавателя');
  RegisterTableField('Teachers_Subjects', 'TeacherID', 'Преподаватель', 'Teachers', 'TeacherID'
    ,'TeacherInitials', 250);
  RegisterTableField('Teachers_Subjects', 'SubjectID', 'Предмет', 'Subjects', 'SubjectID', 'SubjectName', 240);

  RegisterTable('Group_Subjects', 'Предметы групп');
  RegisterTableField('Group_Subjects', 'GroupID', 'Группа', 'Groups', 'GroupID', 'GroupNumber', 55);
  RegisterTableField('Group_Subjects', 'SubjectID', 'Предмет', 'Subjects', 'SubjectID', 'SubjectName', 190);
end.
