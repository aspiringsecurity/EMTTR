import React from 'react'
import ProposalContainer from '../containers/ProposalContainer'
import TribunalsContextProvider from '../context/TribunalsContext'

const ProposalsPage = ({type}) => {
  return (
    <TribunalsContextProvider>
      <ProposalContainer type={type} />
    </TribunalsContextProvider>
  );
}

export default ProposalsPage