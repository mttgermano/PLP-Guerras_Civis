import React from 'react';
import { BrowserRouter as Router, Routes, Route } from 'react-router-dom';

import './../css/App.css';
import MainPage from './MainPage.jsx';
import LoginPage from './LoginPage.jsx';
import RegistPage from './RegistPage.jsx';
import RoomPage from './RoomPage.jsx';
import CreateRoom from './CreateRoom.jsx'
import RoomHome from './RoomHome.jsx'
import { UserContextProvider } from '../contexts/userContext.jsx';

function App() {
  return (
    <Router>
      <div className="App">
        <header className="App-header">
          {/* Your header content */}
        </header>
        <UserContextProvider>
          <Routes>
            <Route path="/" exact element={<MainPage />} />
            <Route path="/login/login_player" element={<LoginPage />} />
            <Route path="/login/create_player" element={<RegistPage />} />
            <Route path="/room/login_room" element={<LoginPage />} />
            <Route path="/room/home" element={<RoomHome />} />
            <Route path="/room/create_room" element={<CreateRoom />} />
            <Route path="/room/game" element={<RoomPage />} />
          </Routes>
        </UserContextProvider>
      </div>
    </Router>
  );
}

export default App;
