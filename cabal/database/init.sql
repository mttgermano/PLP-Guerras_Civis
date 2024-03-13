-- Create Rooms Table
CREATE TABLE Room (
    room_uuid       VARCHAR(36) PRIMARY KEY,
    room_name       VARCHAR(50),
    room_password   VARCHAR(50),
    room_master     VARCHAR(36),
    is_up           BOOLEAN,
    cursed_word     VARCHAR(50),
    FOREIGN KEY (room_master) REFERENCES Player(player_uuid)
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
    player_uuid     VARCHAR(36),
    role_idx        INT,
    is_alive        BOOLEAN,
    votes           INT,
    FOREIGN KEY (player_uuid) REFERENCES Player(player_uuid),
);

CREATE TABLE Roles (
    role_idx    INT PRIMARY KEY,
    role        VARCHAR(50),
    isGood      BOOLEAN
);

INSERT INTO Roles (role_idx, role, isGood) VALUES 
    (1,     "assassin",             0)
    (2,     "aprendiz",             0)
    (3,     "paparazzi",            0)
    (4,     "paralisador",          0)
    (5,     "silenciador",          0)
    (6,     "bruxo",                0)
    (7,     "detetive",             1)
    (8,     "juiz",                 1)
    (9,     "policial",             1)
    (10,    "médico",               1)
    (11,    "aldeão",               1)
    (12,    "Espírito Vingativo",   1)
);

CREATE TABLE RoleKnowledge (
    who_knows       VARCHAR(36) PRIMARY KEY,
    who_is_known    VARCHAR(36) PRIMARY KEY,
    FOREIGN KEY (who_knows)     REFERENCES Player(player_uuid),
    FOREIGN KEY (who_is_known)  REFERENCES Player(player_uuid)
);



pedro       matheus
pedro       victor
pedro
