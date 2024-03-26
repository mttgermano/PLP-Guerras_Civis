import React from "react";

import '../css/room-home.css';
import { Link } from "react-router-dom";

const RoomWaiting = () => {

    return (
        <div className="room-container">
            <div className="room-card">
                <div className="room-button-container">
                    <Link to="/room/game"><button className="create-button">INICIAR JOGO</button></Link>
                </div>
                <div className="room-list-container">
                    <span style={{ fontSize: "1.5rem", fontWeight: "bold" }}>PLAYERS</span>
                    <div className="room-list">
                        <div className="room-list-item">
                            <span>User 1</span>
                            <span>ROOM MASTER</span>
                        </div>
                        <div className="room-list-item">
                            <span>User 2</span>
                            <span>GUEST</span>
                        </div>
                        <div className="room-list-item">
                            <span>User 3</span>
                            <span>GUEST</span>
                        </div>
                        <div className="room-list-item">
                            <span>User 4</span>
                            <span>GUEST</span>
                        </div>
                        <div className="room-list-item">
                            <span>User 5</span>
                            <span>GUEST</span>
                        </div>
                        <div className="room-list-item">
                            <span>User 6</span>
                            <span>GUEST</span>
                        </div>
                        <div className="room-list-item">
                            <span>User 7</span>
                            <span>GUEST</span>
                        </div>
                        <div className="room-list-item">
                            <span>User 8</span>
                            <span>GUEST</span>
                        </div>
                        <div className="room-list-item">
                            <span>User 9</span>
                            <span>GUEST</span>
                        </div>
                        <div className="room-list-item">
                            <span>User 10</span>
                            <span>GUEST</span>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    )
}

export default RoomWaiting;