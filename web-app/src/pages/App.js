import React from 'react';
import { BrowserRouter as Router, Routes, Route } from 'react-router-dom';
import './../css/App.css';

import MainPage     from './MainPage';
import LoginPage    from './LoginPage';
import RegistPage   from './RegistPage.js';

function App() {
  return (
    <Router>
      <div className="App">
        <header className="App-header">
          {/* Your header content */}
        </header>
        <Routes>
          <Route path="/"               exact   element={<MainPage />} />
          <Route path="/login/login_player"     element={<LoginPage />} />
          <Route path="/login/create_player"    element={<RegistPage />} />
          <Route path="/room/login_room"        element={<LoginPage />} />
          <Route path="/room/create_room"       element={<RegistPage />} />
        </Routes>
      </div>
    </Router>
  );
}

export default App;
