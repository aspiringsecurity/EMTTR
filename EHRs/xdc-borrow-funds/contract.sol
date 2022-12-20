pragma solidity ^0.8.4;

contract Base1
{
    function foo() virtual public {}
}

contract Base2
{
    function foo() virtual public {}
}

contract Inherited is Base1, Base2
{
    // Derives from multiple bases defining foo(), so we must explicitly
    // override it
    function foo() public override(Base1, Base2) {}
}

contract SimpleMath {
    function add(uint a, uint b) public pure returns (uint) {
    return a+b;
  }

  function subtract(uint a, uint b) public pure returns (uint) {
    return a-b;
  }

  function multiply(uint a, uint b) public pure returns (uint) {
    return a*b;
  }

	function divide(uint a, uint b) public pure returns (uint) {
    return a/b;
  }
}
