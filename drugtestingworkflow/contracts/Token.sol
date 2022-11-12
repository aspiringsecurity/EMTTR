pragma solidity ^0.4.24;


import "./SafeMath.sol";

contract Token  {
    using SafeMath for uint256;

    string public name;
    string public symbol;
    uint8 public decimals;

    bool public isTokenInitialized;

    mapping (address => uint256) public balances;
    mapping (address => mapping (address => uint256)) private _allowed;


    uint256 public totalSupply;

    constructor() public
    {
        isTokenInitialized = false;
    }

    // todo : allow only owner to invoke the function
    function initiateToken(string _name, string _symbol, uint8 _decimals, uint256 _amount) public {
        //require(!isTokenInitialized,"Token already initialized");
        if (isTokenInitialized != true) {
            require(_amount > 0, "amount has to be greater than 0");
            totalSupply = _amount.mul(10 ** uint256(_decimals));
            name = _name;
            symbol = _symbol;
            decimals = _decimals;
            balances[msg.sender] = totalSupply;
            isTokenInitialized = true;
        }
    } 

    function allowance(address owner, address spender) public view returns (uint256) {
        return _allowed[owner][spender];
    }

    function approve(address spender, uint256 value) public returns (bool) {
        require(spender != address(0));
        _allowed[msg.sender][spender] = value;
        return true;
    }

    function transfer(address to, uint256 value) public returns (bool) {
        _transfer(msg.sender, to, value);
        return true;
    }

    function balanceOf(address owner) public view returns (uint256) {
        return balances[owner];
    }


    function _transfer(address from, address to, uint256 value) internal {
        require(to != address(0));

        balances[from] = balances[from].sub(value);
        balances[to] = balances[to].add(value);
    }

    function _mint(address account, uint256 value) internal {
        require(account != address(0));
        totalSupply = totalSupply.add(value);
        balances[account] = balances[account].add(value);
    }

    function _burn(address account, uint256 value) internal {
        require(account != address(0));

        totalSupply = totalSupply.sub(value);
        balances[account] = balances[account].sub(value);
    }

}