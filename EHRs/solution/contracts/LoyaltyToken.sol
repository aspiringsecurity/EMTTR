/* For HealthChainHack, Sep 2018
 * Author: Xinghua Liang
 * Copyright: BlockTEST LLC
 */
 pragma solidity ^0.4.24;

import "./Owned.sol";
import "./SafeMath.sol";

contract LoyaltyToken is Owned {
    using SafeMath for uint256;

    /* Public variables of the token */
    string  public name;
    string  public symbol;
    uint8   public decimals;
    uint256 public totalSupply;
    uint256 public bonus;
    bool    public isTokenInitialized;

    /* This creates an array with all balances */
    mapping (address => uint256) public balanceOf;
    mapping (address => mapping (address => uint256)) public allowance;

    modifier validToken() {
        require(isTokenInitialized,
                "Not initialized");
        _;
    }

    /* This generates a public event on the blockchain that will notify clients */
    event Transfer(address indexed from, address indexed to, uint256 value);
    event Approval(address indexed tokenOwner, address indexed spender, uint tokens);


    /* Initializes contract with initial supply tokens to the creator of the contract */
    function initiateToken (uint256 _totalSupply, 
                            uint256 _bonus, 
                            string _name, 
                            string _symbol, 
                            uint8 _decimal) public onlyOwner {
        require(!isTokenInitialized,
                "Token initialized");
        totalSupply = _totalSupply;                         // Update total supply
        balanceOf[msg.sender] = _totalSupply.sub(_bonus);   // Give the creator all initial tokens
        bonus = _bonus;                                     // bonus for AI tool providers
        name = _name;                                       // Set the name for display purposes
        symbol = _symbol;                                   // Set the symbol for display purposes
        decimals = _decimal;                                // Amount of decimals for display purposes
        isTokenInitialized = true;                          // Prevent furture initialization
    }

    /* Send coins */
    function transfer(address _to, uint256 _value)
        public validToken {
        balanceOf[msg.sender] = balanceOf[msg.sender].sub(_value);
        balanceOf[_to] = balanceOf[_to].add(_value);
        emit Transfer(msg.sender, _to, _value);
    }

    /* Allow another contract to spend some tokens in your behalf */
    function approve(address _spender, uint256 _value)
        public validToken returns (bool success) {
        allowance[msg.sender][_spender] = _value;
        emit Approval(msg.sender, _spender, _value);
        return true;
    }

    /* A contract attempts to get the coins */
    function transferFrom(address _from, address _to, uint256 _value)
        public validToken returns (bool success) {
        balanceOf[_from] = balanceOf[_from].sub(_value);
        allowance[_from][msg.sender] = allowance[_from][msg.sender].sub(_value);
        balanceOf[_to] = balanceOf[_to].add(_value);
        emit Transfer(_from, _to, _value);
        return true;
    }

    /* Award bonus to an address */
    function transferBonus(address _to, uint256 _value)
        public onlyOwner validToken {
        bonus = bonus.sub(_value);
        balanceOf[_to] = balanceOf[_to].add(_value);
    }
}
