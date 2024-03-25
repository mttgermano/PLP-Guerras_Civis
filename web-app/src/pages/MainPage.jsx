import React from 'react';
import { Link } from 'react-router-dom';

import '../css/main-page.css';

const MainPage = () => {
  return (
    <div className='main-container'>
      <div className='main-card'>
        <div className="main-card-content">
          <h2 className='main-title'>GUERRAS CIVIS</h2>

          <div className='main-buttons-container'>
            <Link to="/login/login_player"><button className='main-button'>LOGIN</button></Link>
            <Link to="/login/create_player"><button className='main-button'>REGISTER</button></Link>
          </div>
        </div>
      </div>
    </div>

    // <div>
    //   <div className="centered-circle">Main Page Circle</div>
    //   <div className="centered-buttons">
    //     <Link to="/login/login_player"><button>Login</button></Link>
    //     <Link to="/login/create_player"><button>Register</button></Link>
    //   </div>
    // </div>
  );
};

export default MainPage;
