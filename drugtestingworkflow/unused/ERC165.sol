pragma solidity ^0.4.24;

import "./IERC165.sol";


contract ERC165 is IERC165 {

    bytes4 private constant _InterfaceId_ERC165 = 0x01ffc9a7;


    mapping(bytes4 => bool) private _supportedInterfaces;

    constructor() internal
    {
        _registerInterface(_InterfaceId_ERC165);
    }

    function supportsInterface(bytes4 interfaceId) external view returns (bool)
    {
        return _supportedInterfaces[interfaceId];
    }
 
    function _registerInterface(bytes4 interfaceId) internal
    {
        require(interfaceId != 0xffffffff);
        _supportedInterfaces[interfaceId] = true;
    }
}