import React, { useState } from 'react';
import { api } from '../services/api';
import '../css/user-auth.css';

const LoginPage = () => {
  const [username, setUsername] = useState('');
  const [password, setPassword] = useState('');

  const handleUsernameChange = (event) => {
    setUsername(event.target.value);
  };

  const handlePasswordChange = (event) => {
    setPassword(event.target.value);
  };

  const loginPlayer = async () => {
    // Create a JSON object with username and password
    const userData = {
      username: username,
      password: password
    };
    await api.post('login/login_player', userData, {
      headers: {
        'Content-Type': 'application/json'
      }
    })
      .then(response => response.data)
      .catch(error => {
        console.error('Error:', error);
      });

    // Send JSON request to endpoint
    // fetch('http://localhost:3000/login/login_player', {
    //   method: 'POST',
    //   headers: {
    //     'Content-Type': 'application/json'
    //   },
    //   body: JSON.stringify(userData)
    // })
    // .then(response => response.json())
    // .then(data => {
    //   // Handle response from server
    //   console.log(data);
    // })
    // .catch(error => {
    //   console.error('Error:', error);
    // });

  };


  return (
    <div className='container'>
      <div className="card">
        <div className="card-content">
          <div className="card-title">
            <h2>LOGIN</h2>
            <div className="underline-title"></div>
          </div>
          <form method="post" className="form">
            <label for="username" style={{ paddingTop: "13px" }}>Username</label>
            <input id="username" type="text" className="form-input" value={username} onChange={handleUsernameChange} />

            <label for="password" style={{ paddingTop: "22px" }} type="password">Password</label>
            <input id="password" type="password" className="form-input" value={password} onChange={handlePasswordChange} />

            <button className="form-button" onClick={loginPlayer}>Login</button>
          </form>
          <a className="change-auth" href="/login/create_player">Don't you have an account yet?</a>
        </div>
      </div>
    </div>
  );
};

export default LoginPage;
