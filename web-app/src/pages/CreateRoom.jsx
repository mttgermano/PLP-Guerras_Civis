import React, { useContext, useState } from "react";

import '../css/creation.css';
import { api } from "../services/api";
import { useNavigate } from "react-router-dom";
import { UserContext } from "../contexts/userContext";

const CreateRoom = () => {
    const [roomName, setRoomName] = useState();

    const { currentUser } = useContext(UserContext);

    const navigate = useNavigate();

    const handleRoomNameChange = (event) => {
        setRoomName(event.target.value);
        console.log(currentUser);
    };

    const handleCreateRoom = async (event) => {
        event.preventDefault();

        // Create a JSON object with player name and room name
        const roomData = {
            pName: currentUser.pName,
            rName: roomName,
            rPassword: "4321"
        };

        // Send JSON request to endpoint
        try {
            const { data } = await api.post('room/create_room', roomData, {
                headers: {
                    'Content-Type': 'application/json'
                }
            });

            navigate(`/room/${roomData.rName}`);

        } catch (error) {
            console.error('Error:', error);
        }
    };


    return (
        <div className='auth-container'>
            <div className="card">
                <div className="auth-card-content">
                    <div className="card-title">
                        <h2>CREATE ROOM</h2>
                        <div className="underline-title"></div>
                    </div>
                    <form method="post" className="form">
                        <label htmlFor="roomName" style={{ paddingTop: "13px" }}>Room Name</label>
                        <input id="roomName" type="text" className="form-input" value={roomName} onChange={handleRoomNameChange} />

                        <button className="form-button" onClick={handleCreateRoom}>CREATE ROOM</button>
                    </form>
                    <a className="change-auth" href="/room/home">Back to rooms</a>
                </div>
            </div>
        </div>
    )
}

export default CreateRoom;