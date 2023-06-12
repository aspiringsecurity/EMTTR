pragma solidity ^0.4.23;
pragma experimental ABIEncoderV2;

contract Users {
  uint count;

  struct User {
    address addr;
    bytes pubKey;
    string name;
  }

  mapping (uint => User) users;

  event UserAdded(uint id);

  constructor() public {
    count = 0;
  }

  function addUser(address _addr, bytes _pubKey, string _name) public {
    // EC public key (without 0x prefix) will be 64 bytes
    assert(_pubKey.length == 64);

    User memory user = User({
      addr: _addr,
      pubKey: _pubKey,
      name: _name
    });

    users[count] = user;
    emit UserAdded(count);
    count++;
  }

  function getUserCount() public view returns (uint _count) {
    return count;
  }

  function getUser(uint id) public view returns (address addr, bytes pubKey, string name) {
    User memory user = users[id];
    return (user.addr, user.pubKey, user.name);
  }
}