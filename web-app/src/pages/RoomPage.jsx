import { useEffect, useState, useContext } from 'react';
import { useLocation } from "react-router-dom";

import { UserContext } from '../contexts/userContext';

import { api } from '../services/api';

import '../css/room.css';

const RoomPage = () => {
    const [players, setPlayers] = useState([]);
    const { currentUser } = useContext(UserContext);

    useEffect(() => {
        const fetchPlayers = async () => {
            api.get('room/home')
                .then(response => setPlayers(response.data.players));
        }
        fetchPlayers();
    });

    return (
        <div className="container">
            <div className="chat-container">
                <h1>Chat Box</h1>
            </div>
            <div className="players-container">
                {players.map(player => {
                    <>
                        <span>{player.name}</span>
                        <button>{player.action}</button>
                    </>
                })}
            </div>
        </div>
    )

    // return (
    //     <div className='container'>
    //         <div className="chat-container">
    //             <h1>Chat Box</h1>
    //         </div>
    //         <div className="players-container">
    //             <div className='player-card'>
    //                 <span>Nome do jogador</span>
    //                 <button>botão de ação</button>
    //             </div>
    //             <div className='player-card'>
    //                 <span>Nome do jogador</span>
    //                 <button>botão de ação</button>
    //             </div>
    //             <div className='player-card'>
    //                 <span>Nome do jogador</span>
    //                 <button>botão de ação</button>
    //             </div>
    //             <div className='player-card'>
    //                 <span>Nome do jogador</span>
    //                 <button>botão de ação</button>
    //             </div>
    //             <div className='player-card'>
    //                 <span>Nome do jogador</span>
    //                 <button>botão de ação</button>
    //             </div>
    //             <div className='player-card'>
    //                 <span>Nome do jogador</span>
    //                 <button>botão de ação</button>
    //             </div>
    //             <div className='player-card'>
    //                 <span>Nome do jogador</span>
    //                 <button>botão de ação</button>
    //             </div>
    //             <div className='player-card'>
    //                 <span>Nome do jogador</span>
    //                 <button>botão de ação</button>
    //             </div>
    //             <div className='player-card'>
    //                 <span>Nome do jogador</span>
    //                 <button>botão de ação</button>
    //             </div>
    //             <div className='player-card'>
    //                 <span>Nome do jogador</span>
    //                 <button>botão de ação</button>
    //             </div>
    //             <div className='player-card'>
    //                 <span>Nome do jogador</span>
    //                 <button>botão de ação</button>
    //             </div>
    //             <div className='player-card'>
    //                 <span>Nome do jogador</span>
    //                 <button>botão de ação</button>
    //             </div>

    //         </div>
    //     </div>
    // )
}

export default RoomPage;