import { createContext, useMemo, useState } from 'react';
import PropTypes from 'prop-types';

const ApplicationContext = createContext({
  appRootFolderId: '',
  updateAppRootFolderId: () => {},
});

export function ApplicationProvider({ children }) {
  const [appRootFolderId, setAppRootFolderId] = useState('');
  const updateAppRootFolderId = (newAppRootFolderId) => {
    setAppRootFolderId(newAppRootFolderId);
  };
  const value = useMemo(() => ({ appRootFolderId, updateAppRootFolderId }), [appRootFolderId]);

  return (
    <ApplicationContext.Provider value={value}>
      {children}
    </ApplicationContext.Provider>
  );
}

ApplicationProvider.propTypes = {
  children: PropTypes.node.isRequired,
};

export default ApplicationContext;
