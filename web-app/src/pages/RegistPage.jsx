import React, { useState, useContext } from 'react';
import { useNavigate } from "react-router-dom";

import { UserContext } from '../contexts/userContext';

import { api } from '../services/api';

import '../css/user-auth.css';

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

  const registerPlayer = async () => {
    // Create a JSON object with username and password
    const userData = {
      pName: pName,
      password: password
    };

    // Send JSON request to endpoint
    await api.post('login/create_player', userData, {
      headers: {
        'Content-Type': 'application/json'
      }
    })
      .then(response => {
        console.log(response.data)
        setCurrentUser(response.data);

        // redirects the user to room
        navigate("/room/home")
      })
      .catch(error => {
        console.error('Error:', error);
      });


    // fetch('http://localhost/login/create_player', {
    //   method: 'POST',
    //   headers: {
    //     'Content-Type': 'application/json'
    //   },
    //   body: JSON.stringify(userData)
    // })
    //   .then(response => response.json())
    //   .then(data => {
    //     // Handle response from server
    //     console.log(data);
    //   })
    //   .catch(error => {
    //     console.error('Error:', error);
    //   });
  };

  return (
    <div className='container'>
      <div className="card">
        <div className="card-content">
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
