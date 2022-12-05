pragma solidity >=0.4.21 <0.6.0;

contract Election {
  address public owner;
  uint[] public commitments;

  constructor() public {
    owner = msg.sender;
  }

  modifier restricted() {
    if (msg.sender == owner) _;
  }

  function addCommitment(uint _commitment) public restricted {
    commitments.push(_commitment);
  }

  function getCommitments() public returns (uint[] memory) {
      return commitments;
  }

}
