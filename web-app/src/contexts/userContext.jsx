import { createContext, useState } from "react";

export const UserContext = createContext({});

export function UserContextProvider({ children }) {
    const [currentUser, setCurrentUser] = useState();


    return (
        <UserContext.Provider value={{ currentUser, setCurrentUser }}>
            {children}
        </UserContext.Provider>
    )
}