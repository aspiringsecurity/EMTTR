import React, { useContext, useEffect, useState } from "react";
import { useParams } from "react-router-dom";
import Navbar from "../components/Navbar";
import ProposalsList from "../components/ProposalComponents/ProposalsList";
import SideBar from "../components/SideBar";
import AboutComponent from "../components/AboutComponent";
import NewProposal from "../components/ProposalComponents/NewProposal";
import Proposal from "../components/ProposalComponents/Proposal";
import { getProposals } from "../api/proposals";
import { scrollToTop } from "../api/utils";
import { TribunalsContext } from "../context/TribunalsContext";

const ProposalContainer = ({ type }) => {
  const { slug, id } = useParams();
  const { tribunals } = useContext(TribunalsContext);
  const [proposals, setProposals] = useState([]);
  const [dao, setDao] = useState(null);
  const [proposal, setProposal] = useState({});

  useEffect(() => {
    scrollToTop();
    let isSubscribed = true;
    const dao = tribunals.find((daoObj) => daoObj.slug === slug);
    setDao(dao);

    if (isSubscribed)
      (async () => {
        const data = await getProposals(); // currently using the same proposal list for all daos
        if (data) setProposals(data);
      })();

    return () => {
      isSubscribed = false;
    };
  }, [tribunals]);
  useEffect(() => {
    let isSubscribed = true;

    if (isSubscribed && id) {
      const data = proposals.find((p) => String(p._id) === id);
      if (data) setProposal(data);
    }
    return () => {
      isSubscribed = false;
    };
  }, [id]);

  if (dao)
    return (
      <>
        <Navbar />
        <div id="content" className="pb-6 pt-28 lg:pt-24">
          <div className="px-0 md:px-4 max-w-7xl mx-auto">
            <SideBar dao={dao} type={type} />
            {type === "about" ? (
              <AboutComponent dao={dao} />
            ) : type === "new" ? (
              <NewProposal dao={dao} />
            ) : type === "proposal" ? (
              <Proposal proposal={proposal} dao={dao} />
            ) : (
              <ProposalsList proposals={proposals} dao={dao} />
            )}
          </div>
        </div>
      </>
    );
};

export default ProposalContainer;
