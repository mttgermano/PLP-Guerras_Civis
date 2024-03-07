-- Create Rooms Table
CREATE TABLE Room (
    room_name VARCHAR(50),
    room_uuid VARCHAR(20) PRIMARY KEY,
    room_password VARCHAR(50)
);

-- Create User Table
CREATE TABLE User (
    user_name VARCHAR(50),
    user_uuid VARCHAR(20) PRIMARY KEY,
    user_password VARCHAR(50),
    current_room VARCHAR(20),
    FOREIGN KEY (current_room) REFERENCES Room(room_uuid)
);

-- Create User Game Data Table
CREATE TABLE UserGameData (
    character_name VARCHAR(50),
    user_uuid VARCHAR(20),
    isAlive BOOLEAN,
    votes INT,
    FOREIGN KEY (user_uuid) REFERENCES User(user_uuid)
);
