import React from 'react';

const InfoContext = React.createContext({
    addressInfo: {},
    fetchUserData: () => {}
});

export default InfoContext;