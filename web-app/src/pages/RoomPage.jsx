import { useEffect, useState, useContext } from 'react';
import { useLocation, useParams } from "react-router-dom";
import { ClipLoader } from "react-spinners";

import { UserContext } from '../contexts/userContext';

import { api } from '../services/api';

import '../css/room-game.css';

const RoomPage = () => {
    const [players, setPlayers] = useState({});
    const [isPlayersLoading, setIsPlayersLoading] = useState(true);
    const [isChatLoading, setIsChatLoading] = useState(true);
    const [roomState, setRoomState] = useState({});
    const [dayCounter, setDayCounter] = useState(0);
    const [playerKnowledge, setPlayerKnowledge] = useState([]);
    const [readyForAction, setReadyForAction] = useState(false);

    const [message, setMessage] = useState('');
    const [chatMessages, setChatMessages] = useState([]);

    const { currentUser } = useContext(UserContext);

    const { rName } = useParams();


    useEffect(() => {
        const getRoomState = async () => {
            setIsPlayersLoading(true);

            const { data } = await api.get(`api/get_room_state/${rName}`);

            if (data.rState === "actionRound") {
                setDayCounter(state => state + 1);
            }

            setRoomState(data);
            setReadyForAction(true);
            setIsPlayersLoading(false);
        }

        const getRoomStateInterval = setInterval(getRoomState, 1000 * 60); // 1 min

        return () => clearInterval(getRoomStateInterval);
    }, []);

    useEffect(() => {
        const getPlayersByRoomName = async () => {
            setIsPlayersLoading(true);

            const { data } = await api.get(`api/get_room_players/${rName}`);

            console.log("PLAYERS: " + JSON.stringify(data));

            setPlayers(data);
            setIsPlayersLoading(false);
        }

        setTimeout(getPlayersByRoomName, 5000);
    }, [roomState]);

    // useEffect(() => {
    //     const getMessages = async () => {
    //         setIsChatLoading(true);

    //         if (roomState !== "endGame") {
    //             // possível problema de travar a aplicação pela espera da requisição ser feita
    //             console.log("CHAT MEESSAGES:" + chatMessages.length)
    //             const { data } = await api.get(`api/get_last_messages/${currentUser.pName}/${chatMessages.length}`);

    //             const newMessages = [];

    //             for (let i = 0; i < data.message; i++) {
    //                 let newMessage = {
    //                     pmName: data.player[i],
    //                     message: data.message[i]
    //                 }
    //                 newMessages.push(newMessage);
    //             }

    //             setChatMessages((state) => {
    //                 return [...state, ...newMessages]
    //             })
    //         }
    //         setIsChatLoading(false);

    //         setTimeout(getMessages, 2000);
    //     }

    //     setTimeout(getMessages, 10000);
    // }, []);

    useEffect(() => {
        const getPlayerKnowledge = async () => {
            setIsPlayersLoading(true);

            const { data } = await api.get(`api/get_player_knowledge/${currentUser.pName}`);

            var newData = [];
            for (let i = 0; i < data.player.length; i++) {
                newData.push([data.player[i], data.role[i]]);
            }

            console.log("PLAYER KNOWLEDGE RESPONSE: " + data);
            console.log("PLAYER KNOWLEDGE ARRAY: " + newData);
            setPlayerKnowledge(newData);

            setIsPlayersLoading(false);
        }
        // getActionsResult();
        setTimeout(getPlayerKnowledge, 6000);

    }, [roomState]);


    const handleAction = async (player) => {
        await api.post('game/running/action', {
            paName: currentUser.pName,
            aReciever: player
        });

        setReadyForAction(false);
    }

    const handleMessageChange = (event) => {
        setMessage(event.target.value);
    }

    const handleSendMessage = async (event) => {
        // event.keyCode == 13
        if (event.key === 'Enter' && message.trim() !== '') {
            const messageData = {
                pmName: currentUser.pName,
                message: message
            }

            await api.post('game/running/send_message', messageData);
            setChatMessages(state => [...state, messageData]);

            setMessage('');
        }
    }

    return (
        <div className='room-game-container'>
            <div className="left-container">
                <div className='infos'>
                    <div className='infos-data'>
                        <span className='infos-data-subtitle'>Round:</span>
                        <strong className='infos-data-title'>{dayCounter}</strong>
                    </div>
                    <div className='infos-data'>
                        <span className='infos-data-subtitle'>Period:</span>
                        <strong className='infos-data-title'>{roomState.rState === "actionRound" ? "Night" : "Day"}</strong>
                    </div>
                    <div className='infos-data'>
                        <span className='infos-data-subtitle'>Players alive:</span>
                        <strong className='infos-data-title'>12</strong>
                    </div>
                </div>
                <div className="chat-container">
                    <h1 className='chat-title'>Global chat</h1>
                    {isChatLoading ? <ClipLoader
                        color={"#FFF"}
                        loading={isChatLoading}
                        size={120}
                        aria-label="Loading Spinner"
                        data-testid="loader" /> : (
                        <>
                            <div className='chat-messages'>
                                {chatMessages.map((message, idx) => {
                                    if (message.pmName === "Sistema")
                                        return <span style={{ "color": "#ff5555" }} key={idx}>{message.pmName}: {message.message}</span>
                                    else <span key={idx}>{message.pmName}: {message.message}</span>
                                })}
                            </div>
                            <input
                                maxLength={70}
                                className="chat-input"
                                placeholder='Message'
                                value={message}
                                onChange={handleMessageChange}
                                onKeyUp={handleSendMessage}
                            />
                        </>
                    )}
                </div>
            </div>

            <div className="players-container">
                {isPlayersLoading ? <ClipLoader
                    color={"#FFF"}
                    loading={isPlayersLoading}
                    size={150}
                    aria-label="Loading Spinner"
                    data-testid="loader"
                /> : (
                    <>
                        {players.rPlayers.map(player => (
                            // player[0] -> pUUID / player[1] -> pName / player[2] -> is_alive
                            <div key={player[1]} className={`player-card ${player[2] ? "" : 'player-card-dead'}`}>
                                {currentUser.pName === player[1] ?
                                    <span style={{ "textAlign": "center" }} className="player-name">{player[1]}<br />(You)</span>
                                    : <span className="player-name">{player[1]}</span>}

                                {playerKnowledge.length > 0 ? playerKnowledge.map(playerKnown => {
                                    if (playerKnown[0] === player[1]) {
                                        return <span key={playerKnown[1]}>{playerKnown[1]}</span>
                                    } else {
                                        return ""
                                    }
                                }) : ""}
                                <div className="buttons">
                                    {currentUser.pName === player[1] ? "" : (
                                        <button className='player-button' onClick={() => handleAction(player[1])}>Action</button>
                                    )}
                                </div>
                            </div>
                        ))}
                    </>
                )}
            </div>
        </div>
    )
}

export default RoomPage;