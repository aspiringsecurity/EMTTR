pragma solidity ^0.5.0;
pragma experimental ABIEncoderV2;

// import "./VoteVault.sol";

contract Election {
 
    address public EC_Head;
    mapping(address => bool) public ec_officials;

    // event Approval(address indexed tokenOwner, address indexed spender, uint tokens);
    // event Transfer(address indexed from, address indexed to, uint tokens);

    string[] public  constituencies;
    string[] public parties;
    // mapping(uint8 => string) public partyName;
    mapping(bytes32 => bool) public hasVoted;
    
    uint public voterCount;
    mapping(string => mapping (string => bytes32[])) public vote_hashes;
    mapping(string => mapping (string => uint)) public result;
    mapping(string => string)  winner;

    // Vault opens/opened at this time
    uint256 public beginTime;
    
    // Vault closes/Closed at this times
    uint256 public endTime;
    
   

    using SafeMath for uint;
    
    modifier onlyEcHead {
        require((msg.sender == EC_Head), "You are not Authorized to make this function Call.");
        _;
    }
    modifier onlyEcOfficial {
        require((ec_officials[msg.sender]), "You are not Authorized to make this function Call.");
        _;
    }


   constructor() public {  
	
    EC_Head = msg.sender;
    ec_officials[msg.sender] = true;
    voterCount = 0;
    }  

    function addEcOfficial (address official) public onlyEcHead {
        ec_officials[official] = true;
    }
    function removeEcOfficial(address official) public onlyEcHead {
        ec_officials[official] = false;
        delete ec_officials[official];
    }

    function addParty (string memory party_name) public onlyEcOfficial {
        // require(!isVaultOpened);
        // require(beginTime >= uint256(now), "Cannot add a party after the elections have started.");
        parties.push(party_name);
    }
    function addConstituency (string memory constituency) public onlyEcOfficial{
        // require(!isVaultOpened);
        // require(beginTime >= uint256(now), "Cannot add a constituency after the elections have started.");
        constituencies.push(constituency);
    }

    function getConstituencies() public view returns(string[] memory){
        return constituencies;
    }
    function getParties() public view returns(string[] memory){
        return parties;
    }

    function addElectionDetails(string[] memory _constituencies, string[] memory _parties) public onlyEcHead {
        constituencies.length = 0;
        parties.length = 0;
        for (uint i=0; i<_constituencies.length; i++){
            constituencies.push(_constituencies[i]);
        }
        for (uint i=0; i<_parties.length; i++){
            parties.push(_parties[i]);
        }
        // constituencies = _constituencies;
        // parties = _parties;
    }

    function registerVote(bytes32 uuid_hash, string memory constituency, string memory party,  bytes32 vote_hash) public {
        // require(!vaultSealed);
        // require(isVaultOpened);
        // require(beginTime <= uint256(now), "The elections haven't started yet");
        // require(endTime >= uint256(now), "The election period is over");
        require(!(hasVoted[uuid_hash]), "The voter has already voted. Aborting.");

        vote_hashes[constituency][party].push(vote_hash);
        voterCount++;
        hasVoted[uuid_hash] = true;
    }

    function calculateVotes() public  onlyEcHead returns (bool) {
        // require(endTime <= uint256(now), "The election period is not over. Can't calculate the votes before election ends.");
        string memory _winner;

        for (uint i = 0; i < constituencies.length; i++)
        {
            _winner = parties[0];
            for (uint j = 0; j < parties.length; j++){
                 result[constituencies[i]][parties[j]] = vote_hashes[constituencies[i]][parties[j]].length;
                if ( result[constituencies[i]][parties[j]] > result[constituencies[i]][_winner]){
                    _winner = parties[j];
                }
            }
             winner[constituencies[i]] = _winner;
        }
        return true;
    }

    function getWinner(string memory constituency) public view returns (string memory) {
        return  winner[constituency];
    }
    function getVoteHashes(string memory constituency, string memory party) public view returns (bytes32[] memory) {
        return  vote_hashes[constituency][party];
    }
    function getVoteCount(string memory constituency, string memory party) public view returns (uint) {
        return  result[constituency][party];
    }

    function resetElection() public onlyEcHead returns(bool){
        for (uint i = 0; i < constituencies.length; i++)
        {
            for (uint j = 0; j < parties.length; j++){
                 result[constituencies[i]][parties[j]] = 0;
                 vote_hashes[constituencies[i]][parties[j]].length = 0;

            }
             winner[constituencies[i]] = "";
        }
        voterCount = 0;
        return true;
    }
}

library SafeMath { 
    function sub(uint256 a, uint256 b) internal pure returns (uint256) {
      assert(b <= a);
      return a - b;
    }
    
    function add(uint256 a, uint256 b) internal pure returns (uint256) {
      uint256 c = a + b;
      assert(c >= a);
      return c;
    }
}
