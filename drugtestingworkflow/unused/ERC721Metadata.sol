pragma solidity ^0.4.24;

import "./ERC721.sol";
import "./IERC721Metadata.sol";
import "./ERC165.sol";

contract ERC721Metadata is ERC165, ERC721, IERC721Metadata {
 
    string private _name;

 
    string private _symbol;

    mapping(uint256 => string) private _tokenURIs;

    bytes4 private constant InterfaceId_ERC721Metadata = 0x5b5e139f;

    constructor(string name, string symbol) public {
        _name = name;
        _symbol = symbol;

        // register the supported interfaces to conform to ERC721 via ERC165
        _registerInterface(InterfaceId_ERC721Metadata);
    }
 
    function name() external view returns (string) {
        return _name;
    }
 
    function symbol() external view returns (string) {
        return _symbol;
    }

 
    function tokenURI(uint256 tokenId) external view returns (string) {
        require(_exists(tokenId));
        return _tokenURIs[tokenId];
    }
 
    function _setTokenURI(uint256 tokenId, string uri) internal {
        require(_exists(tokenId));
        _tokenURIs[tokenId] = uri;
    }

 
    function _burn(address owner, uint256 tokenId) internal {
        super._burn(owner, tokenId);

        if (bytes(_tokenURIs[tokenId]).length != 0) {
            delete _tokenURIs[tokenId];
        }
    }
}