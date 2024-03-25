import React, { useState } from "react";

import '../css/creation.css';
import { api } from "../services/api";
import { useNavigate } from "react-router-dom";

const CreateRoom = () => {
    const [roomName, setRoomName] = useState();

    const navigate = useNavigate();

    const handleRoomNameChange = (event) => {
        setRoomName(event.target.value);
    };

    const createRoom = async (event) => {
        event.preventDefault();

        // Create a JSON object with username and password
        const roomData = {
            roomName: roomName
        };

        // Send JSON request to endpoint
        await api.post('room/create-room', roomData, {
            headers: {
                'Content-Type': 'application/json'
            }
        })
            .then(response => {
                console.log(response.data)

                // redirects user to room
                navigate("/room/home");
            })
            .catch(error => {
                console.error('Error:', error);
            });
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

                        <button className="form-button" onClick={createRoom}>CREATE ROOM</button>
                    </form>
                    <a className="change-auth" href="/room/home">Back to rooms</a>
                </div>
            </div>
        </div>
    )
}

export default CreateRoom;