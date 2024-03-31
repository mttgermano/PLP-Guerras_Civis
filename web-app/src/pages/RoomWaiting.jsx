import React, { useContext, useEffect, useState } from "react";

import '../css/room-home.css';
import { Link, useParams } from "react-router-dom";
import { api } from "../services/api";
import { UserContext } from "../contexts/userContext";

const RoomWaiting = () => {
    const [players, setPlayers] = useState({});
    const [room, setRoom] = useState({});
    const [isLoading, setIsLoading] = useState(true);

    const { currentUser } = useContext(UserContext);

    const { rName } = useParams();

    useEffect(() => {

        const getRoomByName = async () => {
            try {
                const { data } = await api.get(`api/get_room/${rName}`);

                setRoom(data);

            } catch (error) {
                console.log('Error:', error);
            }
        };

        const getPlayersByRoomId = async () => {
            try {
                setIsLoading(true);
                const { data } = await api.get(`api/get_room_players/${rName}`);

                setPlayers(data);

                setIsLoading(false);
            } catch (error) {
                console.log('Error:', error);
            }
        };

        getRoomByName();
        getPlayersByRoomId();
        const fetchInterval = setInterval(getPlayersByRoomId, 7000);

        return () => clearInterval(fetchInterval)
    }, [rName]);

    return (
        <div className="room-container">
            <div className="room-card">
                <div className="room-button-container">
                    {currentUser.pName === room.rMaster ? <Link to={`/room/${rName}/game`}><button className="create-button">START GAME</button></Link> : ""}
                </div>
                <div className="room-list-container">
                    {isLoading ? <span>Loading...</span> : (
                        <>
                            <span style={{ fontSize: "1.5rem", fontWeight: "bold" }}>PLAYERS</span>
                            <div className="room-list">
                                {players.rPlayers.map(player => (
                                    <div className="room-list-item">
                                        <span>{player[1]}</span>
                                        <span>{player[1] === room.rMaster ? "ROOM MASTER" : "GUEST"}</span>
                                    </div>
                                ))}
                            </div>
                        </>
                    )}

                </div>
            </div>
        </div>
    )
}

export default RoomWaiting;