import React, { useState } from 'react';

const RegisterPage = () => {
  const [username, setUsername] = useState('');
  const [password, setPassword] = useState('');

  const handleUsernameChange = (event) => {
    setUsername(event.target.value);
  };

  const handlePasswordChange = (event) => {
    setPassword(event.target.value);
  };

  const registerPlayer = () => {
    // Create a JSON object with username and password
    const userData = {
      username: username,
      password: password
    };

    // Send JSON request to endpoint
    fetch('http://localhost/login/create_player', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify(userData)
    })
    .then(response => response.json())
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
      <div className="centered-circle">Registration Page Circle</div>
      <div className="input-boxes">
        <input type="text"      placeholder="Username" value={username} onChange={handleUsernameChange} />
        <input type="password"  placeholder="Password" value={password} onChange={handlePasswordChange} />
      </div>
      <button onClick={registerPlayer}>Register</button>
    </div>
  );
};

export default RegisterPage;
