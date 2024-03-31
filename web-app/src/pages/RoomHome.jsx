import React, { useContext, useEffect, useState } from "react";

import '../css/room-home.css';
import { Link, useNavigate } from "react-router-dom";
import { api } from "../services/api";
import { UserContext } from "../contexts/userContext";


const RoomHome = () => {
    const [rooms, setRooms] = useState();
    const [isLoading, setIsLoading] = useState(true);

    const { currentUser } = useContext(UserContext);

    const navigate = useNavigate();

    console.log("CURRENT USER" + currentUser.pName);


    useEffect(() => {
        const fetchRooms = async () => {
            try {
                setIsLoading(true);
                const { data } = await api.get('api/get_rooms_list');

                console.log(data);
                setRooms(data);
                setIsLoading(false);
            } catch (error) {
                console.log('Error', error);
            }

        }
        fetchRooms();
    }, []);

    const handleEnterRoom = async (room) => {
        const loginRoomData = {
            pName: currentUser.pName,
            rName: room[0],
        };

        console.log(loginRoomData);

        await api.post('room/login_room', loginRoomData);

        navigate(`/room/${loginRoomData.rName}`);
    }

    return (
        <div className="room-container">
            <div className="room-card">
                <div className="room-button-container">
                    <Link to="/room/create_room"><button className="create-button">CREATE ROOM</button></Link>
                </div>
                <div className="room-list-container">
                    {isLoading ? <span>Loading...</span> : (
                        <>
                            <span style={{ fontSize: "1.5rem", fontWeight: "bold" }}>ROOMS</span>
                            {rooms.rList.length > 0 ? (
                                <div className="room-list">
                                    {rooms.rList.map(room => (
                                        <div key={room[0]} className="room-list-item">
                                            <span>{room[0]}</span>
                                            <span>{room[1]} players</span>
                                            <button className="enter-button" onClick={() => handleEnterRoom(room)}>Enter</button>
                                        </div>
                                    ))}
                                </div>
                            ) : (
                                <span>No Rooms Available<br /> Create the first one!</span>
                            )
                            }
                        </>
                    )}

                </div>
            </div>
        </div>
    );

    // return (
    //     <div className="room-container">
    //         <div className="room-card">
    //             <div className="room-button-container">
    //                 <Link to="/room/create_room"><button className="create-button">CREATE ROOM</button></Link>
    //             </div>
    //             <div className="room-list-container">
    //                 <span style={{ fontSize: "1.5rem", fontWeight: "bold" }}>ROOMS</span>
    //                 <div className="room-list">
    //                     <div className="room-list-item">
    //                         <span>Sala 1</span>
    //                         <span>12 players</span>
    //                         <button className="enter-button">Enter</button>
    //                     </div>
    //                     <div className="room-list-item">
    //                         <span>Sala 2</span>
    //                         <span>12 players</span>
    //                         <button className="enter-button">Enter</button>
    //                     </div>
    //                     <div className="room-list-item">
    //                         <span>Sala 3</span>
    //                         <span>12 players</span>
    //                         <button className="enter-button">Enter</button>
    //                     </div>
    //                     <div className="room-list-item">
    //                         <span>Sala 4</span>
    //                         <span>12 players</span>
    //                         <button className="enter-button">Enter</button>
    //                     </div>
    //                     <div className="room-list-item">
    //                         <span>Sala 4</span>
    //                         <span>12 players</span>
    //                         <button className="enter-button">Enter</button>
    //                     </div>
    //                 </div>
    //             </div>
    //         </div>
    //     </div>
    // )
}

export default RoomHome;