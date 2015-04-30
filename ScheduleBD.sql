CREATE DATABASE 'D:\1FBD\final v2\ScheduleBD.fdb' user 'SYSDBA' password 'masterkey' DEFAULT CHARACTER SET WIN1251;
CREATE TABLE EducActivities
(
    EducID   INTEGER PRIMARY KEY, 
    EducName VARCHAR (100) 
);
CREATE TABLE Teachers
( 
	TeacherID       INTEGER PRIMARY KEY,
	TeacherInitials VARCHAR (100)
);
CREATE TABLE Groups
(
	GroupID     INTEGER PRIMARY KEY,
	GroupNumber VARCHAR (100) ,
	GroupName   VARCHAR (100)  
);
CREATE TABLE Students
( 
	StudentID       INTEGER PRIMARY KEY,
	StudentInitials VARCHAR (100) ,
	GroupID         INTEGER 
);
CREATE TABLE Subjects
( 
	SubjectID   INTEGER PRIMARY KEY,
    SubjectName VARCHAR (100) 
);
CREATE TABLE Audiences
(
    AudienceID     INTEGER PRIMARY KEY,
	AudienceNumber VARCHAR (100)  
);
CREATE TABLE Pairs
(
	PairID     INTEGER PRIMARY KEY,
	PairBegin  VARCHAR (100) ,
	PairEnd    VARCHAR (100) ,
	PairNumber INTEGER  
);
CREATE TABLE WeekDays
(
	WeekDayID     INTEGER PRIMARY KEY,
	WeekDayName   VARCHAR (100) , 
	WeekDayNumber INTEGER  
);
CREATE TABLE Schedules
(
	ScheduleID INTEGER  PRIMARY KEY,
	GroupID    INTEGER,
	WeekDayID  INTEGER,
	PairID     INTEGER,
	SubjectID  INTEGER,
	EducID     INTEGER,
	TeacherID  INTEGER,
	AudienceID INTEGER,
	FOREIGN KEY (GroupID)    REFERENCES Groups(GroupID),
    FOREIGN KEY (WeekDayID)  REFERENCES WeekDays(WeekDayID),
    FOREIGN KEY (PairID)     REFERENCES Pairs(PairID),
	FOREIGN KEY (SubjectID)  REFERENCES Subjects(SubjectID),
	FOREIGN KEY (EducID)     REFERENCES EducActivities(EducID),
	FOREIGN KEY (TeacherID)  REFERENCES Teachers(TeacherID),
	FOREIGN KEY (AudienceID) REFERENCES Audiences(AudienceID)
);
CREATE TABLE Teachers_Subjects
(
    ID INTEGER,
	TeacherID INTEGER,
	SubjectID INTEGER
	FOREIGN KEY (SubjectID)  REFERENCES Subjects(SubjectID),
	FOREIGN KEY (TeacherID)  REFERENCES Teachers(TeacherID)
);
CREATE TABLE Group_Subjects
(
    ID INTEGER,
	GroupID INTEGER,
	SubjectID INTEGER
	FOREIGN KEY (GroupID)    REFERENCES Groups(GroupID),
	FOREIGN KEY (SubjectID)  REFERENCES Subjects(SubjectID)
);

CREATE SEQUENCE MainSequence;
ALTER SEQUENCE MainSequence RESTART WITH 1100;

INSERT INTO EducActivities VALUES (1, 'Лекция');
INSERT INTO EducActivities VALUES (2, 'Практика');

INSERT INTO Teachers VALUES (101, 'Кленин Александр Сергеевич');
INSERT INTO Teachers VALUES (102, 'Туфанов Игорь Евгеньевич');
INSERT INTO Teachers VALUES (103, 'Клевчихин Юрий Александрович');
INSERT INTO Teachers VALUES (104, 'Лудов Игорь Юрьевич ');
INSERT INTO Teachers VALUES (105, 'Жуплев Антон Сергеевич');
INSERT INTO Teachers VALUES (106, 'Пак Геннадий Константинович');
INSERT INTO Teachers VALUES (107, 'Машенцев Владимир Юрьевич');
INSERT INTO Teachers VALUES (108, 'Преподаватель физкультуры');
INSERT INTO Teachers VALUES (109, 'Спорышев Максим Сергеевич');
INSERT INTO Teachers VALUES (110, 'Никольская  Татьяна Владимировна');
INSERT INTO Teachers VALUES (111, 'Крикунова Юлия Анатольевна');
INSERT INTO Teachers VALUES (112, 'Пак С.Б.');
INSERT INTO Teachers VALUES (113, 'Сторожок Е.А.');
INSERT INTO Teachers VALUES (114, 'Давыдов Денис Витальевич');
INSERT INTO Teachers VALUES (115, 'Доставалов Валерий Николаевич');
INSERT INTO Teachers VALUES (116, 'Шепелева Риорита Петровна');
INSERT INTO Teachers VALUES (117, 'Романюк Мария Александровна');
INSERT INTO Teachers VALUES (118, 'Бризицкий Роман Викторович');
INSERT INTO Teachers VALUES (119, 'Пынько Ирина Викторовна');
INSERT INTO Teachers VALUES (120, 'Кравцов Дмитрий Сергеевич');
INSERT INTO Teachers VALUES (121, 'Пак Татьяна Владимировна');

INSERT INTO Groups VALUES (701, 'Б8103a', 'Прикладная математика и информатика');
INSERT INTO Groups VALUES (702, 'Б8103б', 'Прикладная математика и информатика');
INSERT INTO Groups VALUES (703, 'Б8203а', 'Прикладная математика и информатика');
INSERT INTO Groups VALUES (704, 'Б8203б', 'Прикладная математика и информатика');

INSERT INTO Students VALUES (201,  'Акмухамедова Наталья Алексеевна', 701);
INSERT INTO Students VALUES (202,  'Батраков Артем Глебович', 701);
INSERT INTO Students VALUES (203,  'Бедина Полина Артемовна', 701);
INSERT INTO Students VALUES (204,  'Варламов Кирилл Евгеньевич', 701);
INSERT INTO Students VALUES (205,  'Ватутин Сергей Федорович', 701);
INSERT INTO Students VALUES (206,  'Виноградов Иван Владимирович', 701);
INSERT INTO Students VALUES (207,  'Возный Роман Сергеевич', 701);
INSERT INTO Students VALUES (208,  'Глух Валерия Игоревна', 701);
INSERT INTO Students VALUES (209,  'Глущенко Николай Владимирович', 701);
INSERT INTO Students VALUES (210, 'Гнездилова Алина Игоревна', 701);
INSERT INTO Students VALUES (211, 'Гусаров Владислав Евгеньевич', 701);
INSERT INTO Students VALUES (212, 'Гутник Павел Евгеньевич', 701);
INSERT INTO Students VALUES (213, 'Жихарева Александра Руслановна', 701);
INSERT INTO Students VALUES (214, 'Зуев Илья Андреевич', 701);
INSERT INTO Students VALUES (215, 'Карандаев Тимур Александрович', 701);
INSERT INTO Students VALUES (216, 'Кирпа Вадим Дмитриевич', 701);
INSERT INTO Students VALUES (217, 'Козицкая Ирина Сергеевна', 701);
INSERT INTO Students VALUES (218, 'Кулинченко Евгений Марсельевич', 701);
INSERT INTO Students VALUES (219, 'Липов Илья Максимович', 701);
INSERT INTO Students VALUES (220, 'Манякин Алексей Сергеевич', 701);
INSERT INTO Students VALUES (221, 'Мартыненко Лев Константинович', 701);
INSERT INTO Students VALUES (222, 'Махлярчук Андрей Александрович', 701);
INSERT INTO Students VALUES (223, 'Михайлова Евгения Александровна', 701);
INSERT INTO Students VALUES (224, 'Недалюк Игорь Андреевич', 701);
INSERT INTO Students VALUES (225, 'Нестеров Михаил Васильевич', 701);
INSERT INTO Students VALUES (226, 'Николенко Михаил Юрьевич', 701);
INSERT INTO Students VALUES (227, 'Рассказова Дарья Романовна', 701);
INSERT INTO Students VALUES (228, 'Сбоев Константин Павлович', 701);
INSERT INTO Students VALUES (229, 'Стоцкий Олег Владиславович', 701);
INSERT INTO Students VALUES (230, 'Томак Дмитрий Геннадьевич', 701);
INSERT INTO Students VALUES (231, 'Трофимова Ольга Николаевна', 701);
INSERT INTO Students VALUES (232, 'Тышковский Артур Дмитриевич', 701);
INSERT INTO Students VALUES (233, 'Утин Никита Сергеевич', 701);
INSERT INTO Students VALUES (234, 'Чернов Дмитрий Дмитриевич', 701);
INSERT INTO Students VALUES (235, 'Щуров Алексей Андреевич', 701);

INSERT INTO Subjects VALUES (301, 'Математический анализ');
INSERT INTO Subjects VALUES (302, 'Алгебра и геометрия');
INSERT INTO Subjects VALUES (303, 'Практикум на ЭВМ');
INSERT INTO Subjects VALUES (304, 'Физическая культура');
INSERT INTO Subjects VALUES (305, 'Иностранный язык');
INSERT INTO Subjects VALUES (306, 'Базы данных');
INSERT INTO Subjects VALUES (307, 'Дискретная математика');
INSERT INTO Subjects VALUES (308, 'Языки и методы программирования');
INSERT INTO Subjects VALUES (309, 'Экономика');
INSERT INTO Subjects VALUES (310, 'Объектно-ориентированный анализ');
INSERT INTO Subjects VALUES (311, 'Физика');
INSERT INTO Subjects VALUES (312, 'Дифференциальные уравнения');
INSERT INTO Subjects VALUES (313, 'Комплексный анализ');
INSERT INTO Subjects VALUES (314, 'Безопасность жизнедеятельности');
INSERT INTO Subjects VALUES (315, 'Численные методы');
INSERT INTO Subjects VALUES (316, 'Экономическая теория');

INSERT INTO Audiences VALUES (401,  'D734');
INSERT INTO Audiences VALUES (402,  'D549a');
INSERT INTO Audiences VALUES (403,  'D547');
INSERT INTO Audiences VALUES (404,  'D743');
INSERT INTO Audiences VALUES (405,  'D547');
INSERT INTO Audiences VALUES (406,  'D732');
INSERT INTO Audiences VALUES (407,  'D542');
INSERT INTO Audiences VALUES (408, 'D747');
INSERT INTO Audiences VALUES (409, 'физкультурный зал');
INSERT INTO Audiences VALUES (410, 'D738');
INSERT INTO Audiences VALUES (411, 'D820');
INSERT INTO Audiences VALUES (412, 'D548');
INSERT INTO Audiences VALUES (413, 'D549'); 
INSERT INTO Audiences VALUES (414, 'D818');
INSERT INTO Audiences VALUES (415, 'D820');
INSERT INTO Audiences VALUES (416, 'D734a'); 

INSERT INTO Pairs VALUES (501, '8.30',  '10.10', 1);
INSERT INTO Pairs VALUES (502, '10.10', '11.40', 2);
INSERT INTO Pairs VALUES (503, '11.50', '13.20', 3);
INSERT INTO Pairs VALUES (504, '13.30', '15.00', 4);
INSERT INTO Pairs VALUES (505, '15.10', '16.40', 5);
INSERT INTO Pairs VALUES (506, '16.50', '18.20', 6);

INSERT INTO WeekDays VALUES (601, 'Понедельник', 1);
INSERT INTO WeekDays VALUES (602, 'Вторник', 2);
INSERT INTO WeekDays VALUES (603, 'Среда', 3);
INSERT INTO WeekDays VALUES (604, 'Четверг', 4);
INSERT INTO WeekDays VALUES (605, 'Пятница', 5);
INSERT INTO WeekDays VALUES (606, 'Суббота', 6);
INSERT INTO WeekDays VALUES (607, 'Воскресенье', 7);

INSERT INTO Schedules VALUES (1001, 701, 601, 501, 303, 2, 107, 401);
INSERT INTO Schedules VALUES (1002, 701, 601, 502, 303, 2, 107, 401);

INSERT INTO Schedules VALUES (1003, 701, 602, 501, 302, 2, 106, 403);
INSERT INTO Schedules VALUES (1004, 701, 602, 502, 307, 2, 106, 407);
INSERT INTO Schedules VALUES (1005, 701, 602, 503, 301, 2, 103, 408);
INSERT INTO Schedules VALUES (1006, 701, 602, 504, 307, 2, 106, 403);

INSERT INTO Schedules VALUES (1007, 701, 603, 502, 307, 2, 106, 408);
INSERT INTO Schedules VALUES (1008, 701, 603, 503, 302, 1, 106, 408);
INSERT INTO Schedules VALUES (1009, 701, 603, 504, 302, 1, 106, 406);
INSERT INTO Schedules VALUES (1010, 701, 603, 505, 304, 2, 104, 409);

INSERT INTO Schedules VALUES (1011, 701, 604, 501, 308, 2, 109, 401);
INSERT INTO Schedules VALUES (1012, 701, 604, 502, 308, 2, 109, 401);
INSERT INTO Schedules VALUES (1013, 701, 604, 503, 308, 1, 104, 410);

INSERT INTO Schedules VALUES (1014, 701, 605, 502, 301, 1, 103, 407);
INSERT INTO Schedules VALUES (1015, 701, 605, 503, 301, 2, 103, 407);

INSERT INTO Schedules VALUES (1016, 701, 606, 502, 306, 2, 101, 401);
INSERT INTO Schedules VALUES (1017, 701, 606, 503, 306, 1, 101, 410);
INSERT INTO Schedules VALUES (1018, 701, 606, 504, 305, 2, 110, 406);
INSERT INTO Schedules VALUES (1019, 701, 606, 505, 304, 2, 108, 409);


INSERT INTO Schedules VALUES (1020, 702, 601, 501, 307, 2, 106, 411);
INSERT INTO Schedules VALUES (1021, 702, 601, 502, 307, 2, 106, 411);
INSERT INTO Schedules VALUES (1022, 702, 601, 503, 305, 2, 111, 404);
INSERT INTO Schedules VALUES (1023, 702, 601, 504, 302, 2, 106, 412);

INSERT INTO Schedules VALUES (1024, 702, 602, 502, 307, 1, 106, 407);
INSERT INTO Schedules VALUES (1025, 702, 602, 503, 302, 2, 106, 403);

INSERT INTO Schedules VALUES (1026, 702, 603, 503, 301, 2, 103, 405);
INSERT INTO Schedules VALUES (1027, 702, 603, 504, 302, 1, 106, 406);
INSERT INTO Schedules VALUES (1028, 702, 603, 505, 304, 2, 108, 409);

INSERT INTO Schedules VALUES (1029, 702, 604, 502, 301, 2, 103, 405);
INSERT INTO Schedules VALUES (1030, 702, 604, 503, 308, 1, 104, 410);
INSERT INTO Schedules VALUES (1031, 702, 604, 504, 303, 2, 112, 401);
INSERT INTO Schedules VALUES (1032, 702, 604, 505, 303, 2, 112, 401);

INSERT INTO Schedules VALUES (1033, 702, 605, 501, 301, 1, 103, 407);
INSERT INTO Schedules VALUES (1034, 702, 605, 502, 308, 2, 102, 401);
INSERT INTO Schedules VALUES (1035, 702, 605, 503, 308, 2, 102, 401);
INSERT INTO Schedules VALUES (1036, 702, 605, 502, 306, 2, 113, 401);

INSERT INTO Schedules VALUES (1037, 702, 606, 503, 306, 1, 101, 410);
INSERT INTO Schedules VALUES (1038, 702, 606, 505, 304, 2, 108, 409);


INSERT INTO Schedules VALUES (1039, 703, 601, 501, 309, 1, 114, 410);
INSERT INTO Schedules VALUES (1040, 703, 601, 502, 309, 1, 114, 410);
INSERT INTO Schedules VALUES (1041, 703, 601, 503, 310, 1, 105, 406);
INSERT INTO Schedules VALUES (1042, 703, 601, 504, 303, 2, 101, 413);
INSERT INTO Schedules VALUES (1043, 703, 601, 505, 303, 2, 101, 413);

INSERT INTO Schedules VALUES (1044, 703, 602, 505, 304, 2, 108, 409);

INSERT INTO Schedules VALUES (1045, 703, 603, 501, 311, 1, 115, 414);
INSERT INTO Schedules VALUES (1046, 703, 603, 502, 312, 1, 116, 406);
INSERT INTO Schedules VALUES (1047, 703, 603, 503, 312, 2, 116, 406);
INSERT INTO Schedules VALUES (1048, 703, 603, 504, 312, 2, 116, 411);
INSERT INTO Schedules VALUES (1049, 703, 603, 505, 305, 2, 117, 404);

INSERT INTO Schedules VALUES (1050, 703, 604, 501, 313, 1, 103, 407);
INSERT INTO Schedules VALUES (1051, 703, 604, 502, 315, 1, 121, 410);
INSERT INTO Schedules VALUES (1052, 703, 604, 503, 313, 2, 103, 405);
INSERT INTO Schedules VALUES (1053, 703, 604, 504, 315, 2, 118, 401);
INSERT INTO Schedules VALUES (1054, 703, 604, 505, 314, 2, 119, 406);

INSERT INTO Schedules VALUES (1055, 703, 605, 501, 310, 2, 105, 401);
INSERT INTO Schedules VALUES (1056, 703, 605, 502, 310, 2, 105, 401);
INSERT INTO Schedules VALUES (1057, 703, 605, 503, 316, 2, 120, 406);


INSERT INTO Schedules VALUES (1058, 704, 601, 501, 309, 1, 114, 410);
INSERT INTO Schedules VALUES (1059, 704, 601, 502, 309, 1, 114, 410);
INSERT INTO Schedules VALUES (1060, 704, 601, 503, 310, 1, 105, 406);
INSERT INTO Schedules VALUES (1061, 704, 601, 504, 310, 2, 107, 406);
INSERT INTO Schedules VALUES (1062, 704, 601, 505, 310, 2, 107, 406);

INSERT INTO Schedules VALUES (1063, 704, 602, 503, 312, 2, 116, 406);
INSERT INTO Schedules VALUES (1064, 704, 602, 504, 312, 2, 116, 406);
INSERT INTO Schedules VALUES (1065, 704, 602, 505, 304, 2, 108, 409);

INSERT INTO Schedules VALUES (1066, 704, 603, 501, 311, 1, 115, 414);
INSERT INTO Schedules VALUES (1067, 704, 603, 502, 312, 1, 116, 406);
INSERT INTO Schedules VALUES (1068, 704, 603, 504, 305, 2, 117, 412);

INSERT INTO Schedules VALUES (1069, 704, 604, 502, 315, 1, 121, 410);
INSERT INTO Schedules VALUES (1070, 704, 604, 503, 315, 2, 118, 401);
INSERT INTO Schedules VALUES (1071, 704, 604, 504, 313, 2, 103, 405);
INSERT INTO Schedules VALUES (1072, 704, 604, 505, 314, 2, 119, 406);

INSERT INTO Schedules VALUES (1073, 704, 605, 502, 303, 2, 112, 402);
INSERT INTO Schedules VALUES (1074, 704, 605, 503, 316, 2, 120, 406);
INSERT INTO Schedules VALUES (1075, 704, 605, 504, 303, 2, 112, 402);
INSERT INTO Schedules VALUES (1076, 704, 605, 505, 304, 2, 108, 409);


INSERT INTO Teachers_Subjects VALUES (801, 101, 306);
INSERT INTO Teachers_Subjects VALUES (802, 103, 301);
INSERT INTO Teachers_Subjects VALUES (803, 104, 308);
INSERT INTO Teachers_Subjects VALUES (804, 106, 302);
INSERT INTO Teachers_Subjects VALUES (805, 106, 307);
INSERT INTO Teachers_Subjects VALUES (806, 107, 303);
INSERT INTO Teachers_Subjects VALUES (807, 108, 304);
INSERT INTO Teachers_Subjects VALUES (808, 109, 308);
INSERT INTO Teachers_Subjects VALUES (809, 110, 305);
INSERT INTO Teachers_Subjects VALUES (810, 111, 305);
INSERT INTO Teachers_Subjects VALUES (811, 112, 303);
INSERT INTO Teachers_Subjects VALUES (812, 113, 306);
INSERT INTO Teachers_Subjects VALUES (813, 114, 309);
INSERT INTO Teachers_Subjects VALUES (814, 105, 310);
INSERT INTO Teachers_Subjects VALUES (815, 115, 311);
INSERT INTO Teachers_Subjects VALUES (816, 117, 305);
INSERT INTO Teachers_Subjects VALUES (817, 101, 303);
INSERT INTO Teachers_Subjects VALUES (818, 103, 313);
INSERT INTO Teachers_Subjects VALUES (819, 116, 312);
INSERT INTO Teachers_Subjects VALUES (820, 121, 315);
INSERT INTO Teachers_Subjects VALUES (821, 119, 314);
INSERT INTO Teachers_Subjects VALUES (822, 118, 315);
INSERT INTO Teachers_Subjects VALUES (823, 120, 316);

INSERT INTO Group_Subjects VALUES (901, 701, 301);
INSERT INTO Group_Subjects VALUES (902, 701, 302);
INSERT INTO Group_Subjects VALUES (903, 701, 303);
INSERT INTO Group_Subjects VALUES (904, 701, 304);
INSERT INTO Group_Subjects VALUES (905, 701, 305);
INSERT INTO Group_Subjects VALUES (906, 701, 306);
INSERT INTO Group_Subjects VALUES (907, 701, 307);
INSERT INTO Group_Subjects VALUES (908, 701, 308);
INSERT INTO Group_Subjects VALUES (909, 702, 301);
INSERT INTO Group_Subjects VALUES (910, 702, 302);
INSERT INTO Group_Subjects VALUES (911, 702, 303);
INSERT INTO Group_Subjects VALUES (912, 702, 304);
INSERT INTO Group_Subjects VALUES (913, 702, 305);
INSERT INTO Group_Subjects VALUES (914, 702, 306);
INSERT INTO Group_Subjects VALUES (915, 702, 307);
INSERT INTO Group_Subjects VALUES (916, 702, 308);
INSERT INTO Group_Subjects VALUES (917, 703, 309);
INSERT INTO Group_Subjects VALUES (918, 703, 310);
INSERT INTO Group_Subjects VALUES (919, 703, 304);
INSERT INTO Group_Subjects VALUES (920, 703, 311);
INSERT INTO Group_Subjects VALUES (921, 703, 312);
INSERT INTO Group_Subjects VALUES (922, 703, 305);
INSERT INTO Group_Subjects VALUES (923, 703, 313);
INSERT INTO Group_Subjects VALUES (924, 703, 314);
INSERT INTO Group_Subjects VALUES (925, 703, 315);
INSERT INTO Group_Subjects VALUES (926, 703, 303);
INSERT INTO Group_Subjects VALUES (927, 703, 316);
INSERT INTO Group_Subjects VALUES (928, 704, 309);
INSERT INTO Group_Subjects VALUES (929, 704, 310);
INSERT INTO Group_Subjects VALUES (930, 704, 304);
INSERT INTO Group_Subjects VALUES (931, 704, 311);
INSERT INTO Group_Subjects VALUES (932, 704, 312);
INSERT INTO Group_Subjects VALUES (933, 704, 305);
INSERT INTO Group_Subjects VALUES (934, 704, 313);
INSERT INTO Group_Subjects VALUES (935, 704, 314);
INSERT INTO Group_Subjects VALUES (936, 704, 315);
INSERT INTO Group_Subjects VALUES (937, 704, 303);
INSERT INTO Group_Subjects VALUES (938, 704, 316);


