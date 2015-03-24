unit USQLQueries;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UMetadata;

function EditSQlQuery(ACurTab: Integer): String;

implementation

function EditSQlQuery(ACurTab: Integer): String;
var
  i: Integer;
begin
  with Tables[ACurTab] do
  begin
    if (Fields[0] is TMyJoinedField) then
      Result := 'Select ' + (Fields[0] as TMyJoinedField).ReferencedTable
      + '.' + (Fields[0] as TMyJoinedField).JoinedFieldName + ' AS ' + Fields[0].Name
    else
      Result := 'Select ' + Name + '.' + Fields[0].Name;

    for i := 1 to GetFieldsLength - 1 do
      if (Fields[i] is TMyJoinedField) then
        Result += ', ' + (Fields[i] as TMyJoinedField).ReferencedTable
          + '.' + (Fields[i] as TMyJoinedField).JoinedFieldName + ' AS ' + Fields[i].Name
      else
        Result += ', ' + Name + '.' + Fields[i].Name;

    Result += ' FROM ' + Name;

    for i := 0 to GetFieldsLength - 1 do
      if Fields[i] is TMyJoinedField then
        Result += ' INNER JOIN ' + (Fields[i] as TMyJoinedField).ReferencedTable
          + ' ON ' + Name + '.' + Fields[i].Name
          + ' = ' + (Fields[i] as TMyJoinedField).ReferencedTable + '.'
          + (Fields[i] as TMyJoinedField).ReferencedField;
    Result += ' ORDER BY 1, 2';
  end;
end;

end.

