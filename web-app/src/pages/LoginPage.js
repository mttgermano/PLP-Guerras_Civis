import React, { useState } from 'react';

const LoginPage = () => {
  const [username, setUsername] = useState('');
  const [password, setPassword] = useState('');

  const handleUsernameChange = (event) => {
    setUsername(event.target.value);
  };

  const handlePasswordChange = (event) => {
    setPassword(event.target.value);
  };

  const loginPlayer = () => {
    // Create a JSON object with username and password
    const userData = {
      pName: username,
      pPassword: password
    };

    // Send JSON request to endpoint
    fetch('http://localhost:3000/login/create_player', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json'
        },
        body: JSON.stringify(userData)
    })
    .then(response => {
        if (!response.ok) {
            throw new Error('Network response was not ok');
        }
        return response.json();
    })
    .then(data => {
        // Handle response from server
        console.log(data);
    })
    .catch(error => {
        console.error('Error:', error);
    });
  };


  return (
    <div>
      <div className="centered-circle">Login Page Circle</div>
      <div className="input-boxes">
        <input type="text"      placeholder="Username" value={username} onChange={handleUsernameChange} />
        <input type="password"  placeholder="Password" value={password} onChange={handlePasswordChange} />
      </div>
      <button onClick={loginPlayer}>Login</button>
    </div>
  );
};

export default LoginPage;
