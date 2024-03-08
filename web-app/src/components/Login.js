import React from 'react';

const Login = () => {
  const createUser = () => {
    fetch('http://localhost:3000/login/create_player', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      body: JSON.stringify({ username: 'example', password: 'password' })
    })
    .then(response  => response.json())
    .then(data      => console.log(data))
    .catch(error    => console.error('Error:', error));
  };

  return (
    <div>
      <button onClick={createUser}>Create User</button>
      <button>Login User</button>
    </div>
  );
};

export default Login;
