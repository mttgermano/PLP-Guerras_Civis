import React, { useState, useEffect, useContext } from 'react';
import { useLocation } from 'react-router-dom';
import { UserContext } from '../contexts/userContext';
import { api } from '../services/api';

const ChatRoom = () => {
  const [message, setMessage] = useState('');
  const [chatMessages, setChatMessages] = useState([]);
  const { currentUser } = useContext(UserContext);
  const location = useLocation();

  useEffect(() => {
    const fetchChatMessages = async () => {
      try {
        const response = await api.get('chat/messages');
        setChatMessages(response.data.messages);
      } catch (error) {
        console.error('Error fetching chat messages:', error);
      }
    };
    fetchChatMessages();
  }, []);

  const handleInputChange = (event) => {
    setMessage(event.target.value);
  };

  const sendMessage = async (event) => {
    event.preventDefault();
    if (message.trim() !== '') {
      try {
        await api.post('chat/send', {
          username: currentUser.username,
          message: message
        });
        setMessage('');
        // Atualiza as mensagens do chat
        const response = await api.get('chat/messages');
        setChatMessages(response.data.messages);
      } catch (error) {
        console.error('Error sending message:', error);
      }
    }
  };

  return (
    <div className="chat-container">
      <h1 className="chat-title">Global Chat</h1>
      <div className="chat-messages">
        {chatMessages.map((msg, index) => (
          <div key={index} className="message">
            <span className="message-username">{msg.username}:</span> {msg.message}
          </div>
        ))}
      </div>
      <form onSubmit={sendMessage} className="message-form">
        <input type="text" placeholder="Message" value={message} onChange={handleInputChange} />
        <button type="submit">Send</button>
      </form>
    </div>
  );
};

export default ChatRoom;
