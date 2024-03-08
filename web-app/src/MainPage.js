import React from 'react';
import { Link } from 'react-router-dom';

const MainPage = () => {
  return (
    <div>
      <div className="centered-circle">Main Page Circle</div>
      <div className="centered-buttons">
        <Link to="/login/login_player"><button>Login</button></Link>
        <Link to="/login/create_player"><button>Register</button></Link>
      </div>
    </div>
  );
};

export default MainPage;
