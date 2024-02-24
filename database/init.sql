-- Create System Tables
CREATE TABLE "Rooms" (
    room_name VARCHAR(255),
    room_uuid VARCHAR(255),
    room_password VARCHAR(255)
);

CREATE TABLE "Users" (
    user_name VARCHAR(255),
    user_uuid VARCHAR(255),
    user_password VARCHAR(255)
);


-- Create Rooms Table
CREATE TABLE Room (
    user_character VARCHAR(255),
    user_name VARCHAR(255),
    isAlive INT
);

CREATE TABLE Votes (
    user_name VARCHAR(255),
    votes INT
);


-- Examples
INSERT INTO "Rooms" (room_name, room_uuid, room_password) VALUES 
    ('jogo1', '54asdasd1', 'arco'),
    ('jogo2', '22uito104', 'teste');

INSERT INTO "User" (user_name, user_uuid, user_password) VALUES 
    ('pedro', 'asd234asd', 'pass1'),
    ('carlos', '67sf3oo', 'kasco'),
    ('joao', '2342340sd', 'teste'),
    ('val', 'fffffaaaa', 'iiiii'),
    ('dir', '234jdasd0', 'vetor');


-- Rooms Table Data
INSERT INTO Room (user_character, user_name, isAlive) VALUES 
    ('asd1', 'pedro', 1),
    ('asd2', 'carlos', 1),
    ('asd3', 'joao', 0),
    ('asd4', 'val', 0),
    ('asd5', 'dir', 1);

-- Votes Table Data
INSERT INTO Votes (user_name, votes) VALUES 
    ('pedro', 2),
    ('carlos', 0),
    ('joao', 3),
    ('val', 0),
    ('dir', 0);
