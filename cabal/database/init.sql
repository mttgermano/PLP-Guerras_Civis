-- Create Rooms Table
CREATE TABLE Room (
    room_uuid VARCHAR(36) PRIMARY KEY,
    room_name VARCHAR(50),
    room_password VARCHAR(50)
);

-- Create User Table
CREATE TABLE Player (
    player_uuid VARCHAR(36) PRIMARY KEY,
    player_name VARCHAR(50),
    player_password VARCHAR(50),
    current_room VARCHAR(20),
    FOREIGN KEY (current_room) REFERENCES Room(room_uuid)
);

-- Create User Game Data Table
CREATE TABLE UserGameData (
    player_uuid VARCHAR(36),
    character_name VARCHAR(50),
    isAlive BOOLEAN,
    votes INT,
    FOREIGN KEY (player_uuid) REFERENCES Player(player_uuid)
);
