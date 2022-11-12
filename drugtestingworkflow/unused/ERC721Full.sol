pragma solidity ^0.4.24;

import "./ERC721.sol";
import "./ERC721Enumerable.sol";
import "./ERC721Metadata.sol";

contract ERC721Full is ERC721, ERC721Enumerable, ERC721Metadata {
    constructor(string name, string symbol) ERC721Metadata(name, symbol) public {
    }
}