import React from "react";

import '../css/room-home.css';
import { Link } from "react-router-dom";

const RoomHome = () => {

    return (
        <div className="room-container">
            <div className="room-card">
                <div className="room-button-container">
                    <Link to="/room/create_room"><button className="create-button">CRIAR ROOM</button></Link>
                </div>
                <div className="room-list-container">
                    <span style={{ fontSize: "1.5rem", fontWeight: "bold" }}>ROOMS</span>
                    <div className="room-list">
                        <div className="room-list-item">
                            <span>Sala 1</span>
                            <span>12 players</span>
                            <button className="enter-button">Enter</button>
                        </div>
                        <div className="room-list-item">
                            <span>Sala 2</span>
                            <span>12 players</span>
                            <button className="enter-button">Enter</button>
                        </div>
                        <div className="room-list-item">
                            <span>Sala 3</span>
                            <span>12 players</span>
                            <button className="enter-button">Enter</button>
                        </div>
                        <div className="room-list-item">
                            <span>Sala 4</span>
                            <span>12 players</span>
                            <button className="enter-button">Enter</button>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    )
}

export default RoomHome;