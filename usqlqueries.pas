unit USQLQueries;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UMetadata;

function EditSQlQuery(ATableName: String): String;

implementation

function EditSQlQuery(ATableName: String): String;
var
  i, CurTab: Integer;
begin
  for i := 0 to High(Tables) do
    if Tables[i].Name = ATableName then
      CurTab := i;

  if (Tables[CurTab].Fields[0] is TMyJoinedField) then
    Result := 'Select ' + (Tables[CurTab].Fields[0] as TMyJoinedField).JoinedTable
    + '.' + (Tables[CurTab].Fields[0] as TMyJoinedField).VisField + ' AS ' + Tables[CurTab].Fields[0].Name
  else
    Result := 'Select ' + Tables[CurTab].Name + '.' + Tables[CurTab].Fields[0].Name;

  for i := 1 to Tables[CurTab].GetFieldsLength - 1 do
    if (Tables[CurTab].Fields[i] is TMyJoinedField) then
      Result += ', ' + (Tables[CurTab].Fields[i] as TMyJoinedField).JoinedTable
        + '.' + (Tables[CurTab].Fields[i] as TMyJoinedField).VisField + ' AS ' + Tables[CurTab].Fields[i].Name
    else
      Result += ', ' + Tables[CurTab].Name + '.' + Tables[CurTab].Fields[i].Name;

  Result += ' FROM ' + ATableName;

  for i := 0 to Tables[CurTab].GetFieldsLength - 1 do
    if Tables[CurTab].Fields[i] is TMyJoinedField then
      Result += ' INNER JOIN ' + (Tables[CurTab].Fields[i] as TMyJoinedField).JoinedTable
        + ' ON ' + Tables[CurTab].Name + '.' + Tables[CurTab].Fields[i].Name
        + ' = ' + (Tables[CurTab].Fields[i] as TMyJoinedField).JoinedTable + '.'
        + (Tables[CurTab].Fields[i] as TMyJoinedField).JoinedField;
  Result += ' ORDER BY 1, 2';
end;

end.

