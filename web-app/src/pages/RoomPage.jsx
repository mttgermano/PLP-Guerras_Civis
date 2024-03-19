import { useEffect, useState, useContext } from 'react';
import { useLocation } from "react-router-dom";

import { UserContext } from '../contexts/userContext';

import { api } from '../services/api';

import '../css/room.css';

const RoomPage = () => {
    const [players, setPlayers] = useState([]);
    const { currentUser } = useContext(UserContext);

    // useEffect(() => {
    //     const fetchPlayers = async () => {
    //         api.get('room/home')
    //             .then(response => setPlayers(response.data.players));
    //     }
    //     fetchPlayers();
    // });

    // return (
    //     <div className="container">
    //         <div className="chat-container">
    //             <h1>Chat Box</h1>
    //         </div>
    //         <div className="players-container">
    //             {players.map(player => {
    //                 <>
    //                     <span className="player-name">{player.username}</span>
    //                     <div className="buttons">
    //                         <button className='player-button'>{player.action}</button>
    //                         <button className='player-button'>{player.vote}</button>
    //                     </div>
    //                 </>
    //             })}
    //         </div>
    //     </div>
    // )

    return (
        <div className='container'>
            <div className="left-container">
                <div className='infos'>
                    <div className='infos-data'>
                        <span className='infos-data-subtitle'>Round:</span>
                        <strong className='infos-data-title'>1</strong>
                    </div>
                    <div className='infos-data'>
                        <span className='infos-data-subtitle'>Period:</span>
                        <strong className='infos-data-title'>Day</strong>
                    </div>
                    <div className='infos-data'>
                        <span className='infos-data-subtitle'>Players alive:</span>
                        <strong className='infos-data-title'>12</strong>
                    </div>
                </div>
                <div className="chat-container">
                    <h1 className='chat-title'>Global chat</h1>
                    <input className="chat-input" type="text" placeholder='Message' />
                </div>
            </div>

            <div className="players-container">
                <div className='player-card'>
                    <span className="player-name">Username</span>
                    <div className="buttons">
                        <button className='player-button'>Action</button>
                        <button className='player-button'>Vote</button>
                    </div>
                </div>
                <div className='player-card'>
                    <span className="player-name">Username</span>
                    <div className="buttons">
                        <button className='player-button'>Action</button>
                        <button className='player-button'>Vote</button>
                    </div>
                </div>
                <div className='player-card'>
                    <span className="player-name">Username</span>
                    <div className="buttons">
                        <button className='player-button'>Action</button>
                        <button className='player-button'>Vote</button>
                    </div>
                </div>
                <div className='player-card'>
                    <span className="player-name">Username</span>
                    <div className="buttons">
                        <button className='player-button'>Action</button>
                        <button className='player-button'>Vote</button>
                    </div>
                </div>
                <div className='player-card'>
                    <span className="player-name">Username</span>
                    <div className="buttons">
                        <button className='player-button'>Action</button>
                        <button className='player-button'>Vote</button>
                    </div>
                </div>
                <div className='player-card'>
                    <span className="player-name">Username</span>
                    <div className="buttons">
                        <button className='player-button'>Action</button>
                        <button className='player-button'>Vote</button>
                    </div>
                </div>
                <div className='player-card'>
                    <span className="player-name">Username</span>
                    <div className="buttons">
                        <button className='player-button'>Action</button>
                        <button className='player-button'>Vote</button>
                    </div>
                </div>
                <div className='player-card'>
                    <span className="player-name">Username</span>
                    <div className="buttons">
                        <button className='player-button'>Action</button>
                        <button className='player-button'>Vote</button>
                    </div>
                </div>
                <div className='player-card'>
                    <span className="player-name">Username</span>
                    <div className="buttons">
                        <div className='buttons'>
                            <button className='player-button'>Action</button>
                            <button className='player-button'>Vote</button>
                        </div>
                    </div>
                </div>
                <div className='player-card'>
                    <span className="player-name">Username</span>
                    <div className="buttons">
                        <button className='player-button'>Action</button>
                        <button className='player-button'>Vote</button>
                    </div>
                </div>
                <div className='player-card'>
                    <span className="player-name">Username</span>
                    <div className="buttons">
                        <button className='player-button'>Action</button>
                        <button className='player-button'>Vote</button>
                    </div>
                </div>
                <div className='player-card'>
                    <span className="player-name">Username</span>
                    <div className="buttons">
                        <button className='player-button'>Action</button>
                        <button className='player-button'>Vote</button>
                    </div>
                </div>

            </div>
        </div>
    )
}

export default RoomPage;