// SPDX-License-Identifier: MIT
pragma solidity ^0.8.9;

import "@openzeppelin/contracts/token/ERC721/ERC721.sol";
import "@openzeppelin/contracts/token/ERC721/extensions/ERC721Enumerable.sol";
import "@openzeppelin/contracts/token/ERC721/extensions/ERC721URIStorage.sol";
import "@openzeppelin/contracts/security/Pausable.sol";
import "@openzeppelin/contracts/access/Ownable.sol";
import "@openzeppelin/contracts/token/ERC721/extensions/ERC721Burnable.sol";
import "@openzeppelin/contracts/utils/Counters.sol";
import "./SpendSBT.sol";

contract SpendAdmin is ERC721, ERC721Enumerable, ERC721URIStorage, Pausable, Ownable, ERC721Burnable {
    using Counters for Counters.Counter;
    SpendSBT public spendSbtContract;
    uint256 public decryptRate = 0.01 ether;


    Counters.Counter private _tokenIdCounter;

    event KeyAdded(uint256 tokenId, string key);
    event SendingKey(uint256 tokenId, string key);

    struct DecryptionKey {
        string decryptionKey;
    }


    constructor(address spendSbtAddress) ERC721("SpendAdmin", "SPTA") {
        safeMint(msg.sender, "");
        spendSbtContract = SpendSBT(spendSbtAddress);
    }

    mapping (uint => string) private endUserTokenIdToEncryptionKey;

    function _baseURI() internal pure override returns (string memory) {
        return "https://ccdao.mypinata.cloud/ipfs/";
    }

    function pause() public onlyOwner {
        _pause();
    }

    function unpause() public onlyOwner {
        _unpause();
    }

    function addEncryptionKey(uint256 tokenId, string memory encryptionKey) public {
        require(msg.sender == address(spendSbtContract), 'Protected function only invokable by sibling contract.');
        endUserTokenIdToEncryptionKey[tokenId] = encryptionKey;
        emit KeyAdded(tokenId, encryptionKey);
    }

    function safeMint(address to, string memory uri) public onlyOwner {
        uint256 tokenId = _tokenIdCounter.current();
        _tokenIdCounter.increment();

        _mint(to, tokenId);
        _setTokenURI(tokenId, uri);
    }

    function decrypt(uint256[] memory tokenIds) public payable returns (DecryptionKey[] memory) {
        require(balanceOf(msg.sender) > 0, 'Only admins can decrypt');
        require(msg.value > decryptRate * tokenIds.length, 'Not enough funds in treasury.');
        DecryptionKey[] memory decryptionKeys = new DecryptionKey[](tokenIds.length);
        for (uint256 idx = 0; idx < tokenIds.length; idx++) {
            uint256 tokenId = tokenIds[idx];
            address tokenOwner = spendSbtContract.fetchHolder(tokenId);
            payable(tokenOwner).transfer(decryptRate);
            string memory decryptionKey = endUserTokenIdToEncryptionKey[tokenId];
            decryptionKeys[idx].decryptionKey = decryptionKey;
            emit SendingKey(tokenId, decryptionKey);
        }
        spendSbtContract.decrypt(tokenIds);
        return decryptionKeys;
    }

    function addProposal(string memory proposalId) public {
        require(balanceOf(msg.sender) > 0, 'Only admins can add proposals');
        spendSbtContract.addProposal(proposalId);
    }

    function deleteProposal(uint256 index) public {
        require(balanceOf(msg.sender) > 0, 'Only admins can delete proposals');
        spendSbtContract.deleteProposal(index);
    }

    function deposit() public payable {}

    // The following functions are overrides required by Solidity.
    function _beforeTokenTransfer(
        address from,
        address to,
        uint256 /*tokenId*/,
        uint256 /*batchSize*/
    ) internal whenNotPaused view override(ERC721, ERC721Enumerable) {
        require(
            from == address(0) || to == address(0),
            "This a Soulbound token. It cannot be transferred. It can only be burned by the token owner."
        );        
    }

    function _burn(uint256 tokenId) internal override(ERC721, ERC721URIStorage) {
        super._burn(tokenId);
    }

    function totalSupply() public view override returns (uint256) {
        return _tokenIdCounter.current();
    }

    function tokenURI(uint256 tokenId)
        public
        view
        override(ERC721, ERC721URIStorage)
        returns (string memory)
    {
        return super.tokenURI(tokenId);
    }

    function supportsInterface(bytes4 interfaceId)
        public
        view
        override(ERC721, ERC721Enumerable)
        returns (bool)
    {
        return super.supportsInterface(interfaceId);
    }
}