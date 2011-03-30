
DROP TABLE estimates;

CREATE TABLE estimates (
    id SERIAl,
    priority int,
    description varchar(80),
    min int,
    best int,
    max int
);

INSERT INTO estimates (priority, description, min, best, max) VALUES (1, 'first test', 2, 4, 8);
INSERT INTO estimates (priority, description, min, best, max) VALUES (2, 'second test', 3, 4, 8);
INSERT INTO estimates (priority, description, min, best, max) VALUES (3, 'third test', 2, 7, 8);
INSERT INTO estimates (priority, description, min, best, max) VALUES (4, 'fourth test', 2, 4, 10);