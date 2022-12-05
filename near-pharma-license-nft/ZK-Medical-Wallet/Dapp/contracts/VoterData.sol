pragma solidity ^0.5.0;


contract VoterData {
    
    struct Voter {
        string Name;
        uint256 dob;
        string voterId;
        address verifier;
        uint256 verificationTime;
    }

    address public EC_Head;
    mapping(address => bool) public kyc_verifiers;
    mapping(address => bool) public ec_officials;

    mapping(bytes32 => Voter) public voters;
    // mapping(uint256 => Voter) public voters;

    uint256 public verified_voter_count;
   
    using SafeMath for uint256;
    
    modifier onlyEcHead {
        require((msg.sender == EC_Head), "You are not Authorized to make this function Call.");
        _;
    }
    modifier onlyEcOfficial {
        require((ec_officials[msg.sender]), "You are not Authorized to make this function Call.");
        _;
    }
    modifier onlyKycVerifier {
        require((kyc_verifiers[msg.sender]), "You are not Authorized to make this function Call.");
        _;
    }

   constructor() public {  
	
    EC_Head = msg.sender;
    ec_officials[msg.sender] = true;
    kyc_verifiers[msg.sender] = true;
    verified_voter_count = 0;

    }  

    function addEcOfficial (address official) public onlyEcHead {
        ec_officials[official] = true;
    }
    function removeEcOfficial(address official) public onlyEcHead {
        ec_officials[official] = false;
        delete ec_officials[official];
    }
    function addKycVerifier (address verifier) public onlyEcOfficial {
        kyc_verifiers[verifier] = true;
    }
    function removeKycVerifier(address verifier) public onlyEcOfficial {
        kyc_verifiers[verifier] = false;
        delete kyc_verifiers[verifier];
    }
    
    function kycVerify(bytes32  uuid_hash, string memory name, uint256 dob, uint256 current_timestamp) public onlyKycVerifier {
        Voter memory voter = Voter(name, dob, "", msg.sender, current_timestamp);
        voters[uuid_hash] = voter;
        verified_voter_count++;
    }

    function generateVoterId(bytes32 uuid_hash, string memory voterId, uint256 current_timestamp) public {
        
        Voter memory voter = voters[uuid_hash];
        uint256 dob = voter.dob;
        uint256 age = (current_timestamp - dob)/(1000*60*60*24*365);
        
        require(age >= 18, "Not Allowed to Vote. Only eligible to vote if age is above 18 years."); 
        voter.voterId = voterId;
        voters[uuid_hash] = voter;

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
