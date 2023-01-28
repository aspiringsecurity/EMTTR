// SPDX-License-Identifier: MIT
pragma solidity ^0.8.9;

import "@openzeppelin/contracts/token/ERC721/extensions/ERC721URIStorage.sol";
import "@openzeppelin/contracts/utils/Counters.sol";
import "@openzeppelin/contracts/utils/Strings.sol";
import "@openzeppelin/contracts/utils/Base64.sol";
import "@openzeppelin/contracts/security/ReentrancyGuard.sol";
import "@openzeppelin/contracts/access/Ownable.sol";
import "@openzeppelin/contracts/token/ERC721/extensions/ERC721Burnable.sol";
import "./SpendAdmin.sol";

contract SpendSBT is ERC721URIStorage, ERC721Burnable, ReentrancyGuard, Ownable {
    using Strings for uint256;
    using Counters for Counters.Counter;
    Counters.Counter private _tokenIds;
    Counters.Counter private _proposalIdCounter;
    SpendAdmin public spendAdminContract;

    constructor() ERC721("SpendSBT", "DATA") {}

    mapping(uint256 => nft) private tokenIdToNft;
    mapping(address => uint256) public ownerToTokenId;    
    mapping (uint => address) tokenIdToOwner;
    mapping(uint256 => proposal) public activeProposals;

    struct nft {     
        string imageUrl;
        string encryptedCid;
        bool isDecrypted;
    }

    struct proposal {
        uint256 index;
        string proposalId;
        bool isActive;
    }

    function setAdminContract(address adminContractAddress) onlyOwner public {
        spendAdminContract = SpendAdmin(adminContractAddress);
    }

    function getTokenURI(
        uint256 tokenId,
        string memory imageUrl
    ) private pure returns (string memory) {
        bytes memory dataURI = abi.encodePacked("{", '"name": "SPN DAO #', tokenId.toString(), '",', '"image": "', imageUrl, '",', '"description": "A soul-bound token issued by SPN DAO allowing you to take control of and monetize your data."}');
        return string(abi.encodePacked("data:application/json;base64,", Base64.encode(dataURI)));
    }

    function mintLitSBT(                
        string memory imageUrl,
        string memory encryptedCid,
        string memory symmetricKey
    ) public nonReentrant {
        _tokenIds.increment();
        uint256 newNftTokenId = _tokenIds.current();
        _safeMint(msg.sender, newNftTokenId);
        _setTokenURI(newNftTokenId, getTokenURI(newNftTokenId, imageUrl));
        tokenIdToNft[newNftTokenId] = nft(imageUrl, encryptedCid, false);
        ownerToTokenId[msg.sender] = newNftTokenId;
        tokenIdToOwner[newNftTokenId] = msg.sender;
        spendAdminContract.addEncryptionKey(newNftTokenId, symmetricKey);
    }

    function decrypt(uint256[] memory tokenIds) public {
        require(msg.sender == address(spendAdminContract), 'Only invokable by sibling contract.');
        for (uint256 i = 0; i < tokenIds.length; i++) {
            nft memory currNft = tokenIdToNft[tokenIds[i]];
            if (!currNft.isDecrypted && bytes(currNft.encryptedCid).length != 0) {
                currNft.isDecrypted = true;
                tokenIdToNft[tokenIds[i]] = currNft;
            }
        }
    }

    function fetchProposalIds() public view returns (proposal[] memory) {
        proposal[] memory proposalIds = new proposal[](_proposalIdCounter.current());
        for (uint256 idx = 0; idx < _proposalIdCounter.current(); idx++) {
            if (activeProposals[idx].isActive) {
                proposalIds[idx] = activeProposals[idx];
            }
        }
        return proposalIds;
    }

    function addProposal(string memory newProposalId) public {
        require(msg.sender == address(spendAdminContract), 'Only invokable by sibling contract.');
        uint256 index = _proposalIdCounter.current();
        _proposalIdCounter.increment();
        activeProposals[index] = proposal(index, newProposalId, true);
    }

    function deleteProposal(uint256 index) public {
        require(msg.sender == address(spendAdminContract), 'Only invokable by sibling contract.');
        activeProposals[index].isActive = false;
    }

    // Fetch all the NFTs to display
    function fetchNfts() public view returns (nft[] memory) {
        nft[] memory nfts = new nft[](_tokenIds.current());
        for (uint256 idx = 1; idx < _tokenIds.current() + 1; idx++) {
            nft memory currNft = tokenIdToNft[idx];
            nfts[idx - 1] = currNft;
        }
        return nfts;
    }

    function fetchHolders() public view returns (address[] memory) {
        address[] memory holders = new address[](_tokenIds.current());
        for (uint256 idx = 1; idx < _tokenIds.current() + 1; idx++) {
            address currHolder = tokenIdToOwner[idx];
            holders[idx - 1] = currHolder;
        }
        return holders;
    }

    function fetchHolder(uint256 tokenId) public view returns (address) {
        require(msg.sender == address(spendAdminContract), 'Only invokable by sibling contract.');
        return tokenIdToOwner[tokenId];
    }

    function _beforeTokenTransfer(
        address from,
        address to,
        uint256, /*tokenId*/
        uint256 /*batchSize*/
    ) internal pure override(ERC721) {
        require(from == address(0) || to == address(0), "This a Soulbound token. It cannot be transferred. It can only be burned by the token owner.");
    }

    function userBurn(uint256 tokenId) public {
        require(ownerOf(tokenId) == msg.sender, "You do not own this SBT");
        _burn(tokenId);                        
    }

    function _burn(uint256 tokenId) internal override(ERC721, ERC721URIStorage) {
        super._burn(tokenId);
        delete tokenIdToNft[tokenId];
        delete tokenIdToOwner[tokenId];
        delete ownerToTokenId[msg.sender];
    }

    function tokenURI(uint256 tokenId) public view override(ERC721, ERC721URIStorage) returns (string memory) {
        return super.tokenURI(tokenId);
    }
}
