import React, { useState, useContext } from 'react';
import { useNavigate } from "react-router-dom";

import { UserContext } from '../contexts/userContext';

import { api } from '../services/api';

import '../css/creation.css';

const RegisterPage = () => {
  const [pName, setPName] = useState('');
  const [password, setPassword] = useState('');

  const { setCurrentUser } = useContext(UserContext);

  const navigate = useNavigate();

  const handleUsernameChange = (event) => {
    setPName(event.target.value);
  };

  const handlePasswordChange = (event) => {
    setPassword(event.target.value);
  };

  const registerPlayer = async (event) => {
    event.preventDefault();

    // Create a JSON object with username and password
    const userData = {
      pName: pName,
      pPassword: password
    };

    // Send JSON request to endpoint
    try {
      const { data } = await api.post('login/create_player', userData, {
        headers: {
          'Content-Type': 'application/json'
        }
      });
      console.log(data);
      setCurrentUser(data);
      navigate('/room/home');
    } catch (error) {
      console.error('Error:', error);
    }

  };

  return (
    <div className='auth-container'>
      <div className="card">
        <div className="auth-card-content">
          <div className="card-title">
            <h2>REGISTER</h2>
            <div className="underline-title"></div>
          </div>
          <form method="post" className="form">
            <label htmlFor="username" style={{ paddingTop: "13px" }}>Username</label>
            <input id="username" type="text" className="form-input" value={pName} onChange={handleUsernameChange} />

            <label htmlFor="password" style={{ paddingTop: "22px" }} type="password">Password</label>
            <input id="password" type="password" className="form-input" value={password} onChange={handlePasswordChange} />

            <button className="form-button" onClick={registerPlayer}>REGISTER</button>
          </form>
          <a className="change-auth" href="/login/login_player">Already have an account?</a>
        </div>
      </div>
    </div>
  );
};

export default RegisterPage;
