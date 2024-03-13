-- Create Rooms Table
CREATE TABLE Room (
    room_uuid       VARCHAR(36) PRIMARY KEY,
    room_name       VARCHAR(50),
    room_password   VARCHAR(50),
    room_master     VARCHAR(36),
    is_up           BOOLEAN,
    cursed_word     VARCHAR(50)
);

-- Create User Table
CREATE TABLE Player (
    player_uuid     VARCHAR(36) PRIMARY KEY,
    player_name     VARCHAR(50),
    player_password VARCHAR(50),
    current_room    VARCHAR(20),
    FOREIGN KEY (current_room) REFERENCES Room(room_uuid)
);

-- Create User Game Data Table
CREATE TABLE UserGameData (
    player_uuid             VARCHAR(36),
    role_idx                INT,
    is_alive                BOOLEAN,
    votes                   INT,
    kill_vote               INT,
    is_paralized            BOOLEAN,
    is_silenced             BOOLEAN,
    is_dead_by_cursed_word  BOOLEAN,
    FOREIGN KEY (player_uuid) REFERENCES Player(player_uuid)
);

CREATE TABLE Roles (
    role_idx    INT PRIMARY KEY,
    role        VARCHAR(50),
    isGood      BOOLEAN
);

INSERT INTO Roles (role_idx, role, isGood) VALUES 
    (1,     'assassino',            false),
    (2,     'aprendiz',             false),
    (3,     'paparazzi',            false),
    (4,     'paralisador',          false),
    (5,     'silenciador',          false),
    (6,     'bruxo',                false),
    (7,     'detetive',             true),
    (8,     'juiz',                 true),
    (9,     'policial',             true),
    (10,    'medico',               true),
    (11,    'aldeao',               true),
    (12,    'espirito_vingativo',   true);

CREATE TABLE RoleKnowledge (
    who_knows       VARCHAR(36),
    who_is_known    VARCHAR(36),
    FOREIGN KEY (who_knows)     REFERENCES Player(player_uuid),
    FOREIGN KEY (who_is_known)  REFERENCES Player(player_uuid),
    PRIMARY KEY (who_knows, who_is_known)
);
