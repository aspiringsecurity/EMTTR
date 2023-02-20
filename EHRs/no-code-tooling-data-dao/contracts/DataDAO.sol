// first deploy factory contract
// then we need to deploy DataDao contract
// then put contract address on MockMarket

// SPDX-License-Identifier: UNLICENSED

pragma solidity ^0.8.13;

// import {StdStorage} from "../lib/forge-std/src/Components.sol";
import {specific_authenticate_message_params_parse, specific_deal_proposal_cbor_parse} from "./CBORParse.sol";

contract MockMarket {

    DataDAO client;

    constructor(address _client) {
        client = DataDAO(_client);
    }

    function publish_deal(bytes calldata raw_auth_params, uint256 proposalID) public {
        // calls standard filecoin receiver on message authentication api method number
        client.handle_filecoin_method(0, 2643134072, raw_auth_params, proposalID);
    }

}

contract DataDAO {
    uint64 constant public AUTHORIZE_MESSAGE_METHOD_NUM = 2643134072; 
    // number of proposals currently in DAO
    uint256 public proposalCount;
    // mapping to check whether the cid is set for voting 
    mapping(bytes => bool) public cidSet;
    // storing the size of the cid
    mapping(bytes => uint) public cidSizes;
    // 
    mapping(bytes => mapping(bytes => bool)) public cidProviders;

    // address of the owner of DataDAO
    address public immutable owner;

    struct Proposal {
        uint256 proposalID;
        address storageProvider;
        bytes cidraw;
        uint size;
        uint256 upVoteCount;
        uint256 downVoteCount;
        uint256 proposedAt;
        uint256 proposalExpireAt;
    }

    // mapping to keep track of proposals
     mapping(uint256 => Proposal) public proposals;

    // mapping array to track whether the user has voted for the proposal
    mapping(address => mapping(uint256 => bool)) public hasVotedForProposal;

/**
 * @dev constructor: to set the owner address
 */
constructor(address _owner) {
     require(_owner != address(0), "invalid owner!");
     owner = _owner;
}

/***
 * @dev function to create new proposal
 */
    function createCIDProposal(bytes calldata cidraw, uint size) public {
        proposalCount++;
        Proposal memory proposal = Proposal(proposalCount, msg.sender, cidraw, size, 0, 0, block.timestamp, block.timestamp + 1 hours);
        proposals[proposalCount] = proposal;
        cidSet[cidraw] = true;
        cidSizes[cidraw] = size;
    }

    /**
     * @dev function to vote in favour of proposal
     */
    function upvoteCIDProposal(uint256 proposalID) public {
        require(!isCallerSP(proposalID), "Storage Provider cannot vote his own proposal");
        require(!hasVotedForProposal[msg.sender][proposalID], "Already Voted");
        require(isVotingOn(proposalID), "Voting Period Finished");
        proposals[proposalID].upVoteCount = proposals[proposalID].upVoteCount + 1;
        hasVotedForProposal[msg.sender][proposalID] = true;
    }

    /**
     * @dev function to vote in favour of proposal
     */
    function downvoteCIDProposal(uint256 proposalID) public {
        require(!isCallerSP(proposalID), "Storage Provider cannot vote his own proposal");
        require(!hasVotedForProposal[msg.sender][proposalID], "Already Voted");
        require(isVotingOn(proposalID), "Voting Period Finished");
        proposals[proposalID].downVoteCount = proposals[proposalID].downVoteCount + 1;
        hasVotedForProposal[msg.sender][proposalID] = true;
    }

    /**
     * @dev function to check whether the policy is accepted or not
     */
    function policyOK(uint256 proposalID) public view returns (bool) {
        require(proposals[proposalID].proposalExpireAt > block.timestamp, "Voting in On");
        return proposals[proposalID].upVoteCount > proposals[proposalID].downVoteCount;
    }

    /**
     * @dev function to authorizedata and store on filecoin
     */
    function authorizeData(uint256 proposalID, bytes calldata cidraw, bytes calldata provider, uint size) public {
        require(cidSet[cidraw], "CID must be added before authorizing");
        require(cidSizes[cidraw] == size, "Data size must match expected");
        require(policyOK(proposalID), "Deal failed policy check: Was the CID proposal Passed?");
        cidProviders[cidraw][provider] = true;
    }

    /**
     * @dev function to handle filecoin
     */
    function handle_filecoin_method(uint64, uint64 method, bytes calldata params, uint256 proposalID) public {
        // dispatch methods
        if (method == AUTHORIZE_MESSAGE_METHOD_NUM) {
            bytes calldata deal_proposal_cbor_bytes = specific_authenticate_message_params_parse(params);
            (bytes calldata cidraw, bytes calldata provider, uint size) = specific_deal_proposal_cbor_parse(deal_proposal_cbor_bytes);
            cidraw = bytes(bytes(cidraw));
            authorizeData(proposalID, cidraw, provider, size);
        } else {
            revert("The Filecoin method that was called is not handled");
        }
    }

    // getter function also used in require statement

    /**
     * @dev function to get the storage provider address
     */
     function getSP(uint256 proposalID) view public returns(address) {
        return proposals[proposalID].storageProvider;
    }

    /**
     * @dev function to check whether the function caller is the storage provider
     */
    function isCallerSP(uint256 proposalID) view public returns(bool) {
       return getSP(proposalID) == msg.sender;
    }

    /**
     * @dev function to check whether users can start voting on the proposal
     */
    function isVotingOn(uint256 proposalID) view public returns(bool) {
       return proposals[proposalID].proposalExpireAt > block.timestamp;
    }

    /**
     * @dev get the address of this contract
     */
    function getAddressOfContract() public view returns (address) {
        return address(this);
    }
}
