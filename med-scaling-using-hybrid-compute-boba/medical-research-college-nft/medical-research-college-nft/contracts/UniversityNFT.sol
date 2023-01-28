//SPDX-License-Identifier: MIT
pragma solidity ^0.8.9;

import "@openzeppelin/contracts-upgradeable/token/ERC1155/ERC1155Upgradeable.sol";
import "@openzeppelin/contracts-upgradeable/access/OwnableUpgradeable.sol";
import "./ITuringHelper.sol";
import "@openzeppelin/contracts/utils/Strings.sol";
import "base64-sol/base64.sol";

contract UniversityNFT is ERC1155Upgradeable, OwnableUpgradeable {

    struct AlumnusData {
        string linkedInUser;

        // Needed for changeWallet, otherwise Owner would need to provide it.
        uint256[] heldTokenIDs;
    }

    /** @dev Needed for tokenURI to check if it exists & for display. */
    mapping(uint256 => string) private _majorByTokenID;
    mapping(address => AlumnusData) public alumniData;

    /** @dev Instead of using bytes, to avoid having long major names and years being cut off
     *  by the max length of uint256 when using it as tokenID directly. */
    mapping(string => uint256) public tokenIDByMajorYear;
    uint256 nextTokenId;

    ITuringHelper public turingHelper;
    string public imgUri;
    string private _turingUri;

    function initialize(string memory tokenUri_, string memory turingUri_, string memory imgUri_, address turingHelper_) public initializer {
        __Ownable_init();
        ERC1155Upgradeable.__ERC1155_init(tokenUri_);

        turingHelper = ITuringHelper(turingHelper_);
        imgUri = imgUri_;
        _turingUri = turingUri_;
        nextTokenId = 1; // by default 1, to be able to check for empty
    }

    function getTokenID(string memory major_) private returns (uint256) {
        uint256 tokenID = tokenIDByMajorYear[major_];
        if (tokenID == 0) {
            tokenID = nextTokenId++;
            tokenIDByMajorYear[major_] = tokenID;
            _majorByTokenID[tokenID] = major_;
        }
        return tokenID;
    }

    /** @dev Refreshes student data and mints new degrees automatically.
    * Please note that at this point, degrees have to be revoked manually for efficiency & simplicity reasons.
    * Also keep in mind, that the next time someone calls loadStudentData(), the revoked degree shouldn't be returned by the API.
    */
    function refreshAlumnus(address wallet_) public {
        bytes memory encRequest = abi.encode(wallet_);
        bytes memory byteRes = turingHelper.TuringTxV1(_turingUri, encRequest);

        // Major should include YEAR! --> "Computer Science 2022"
        (string[] memory majors, string memory linkedInUser) = abi.decode(byteRes, (string[], string));

        uint256[] storage tokenIDs = alumniData[wallet_].heldTokenIDs;
        for (uint256 i = 0; i < majors.length; i++) {
            uint256 tokenID = getTokenID(majors[i]);

            if (balanceOf(wallet_, tokenID) == 0) {
                _mint(wallet_, tokenID, 1, "");
                tokenIDs.push(tokenID);
            }
        }

        alumniData[wallet_] = AlumnusData(linkedInUser, tokenIDs);
    }

    /** @dev Needs to be revoked manually, since checking for a possible revoke on each reload would be very inefficient
    * and most of the time not necessary.
    */
    function revokeDegree(address alumnusWallet_, uint256[] calldata majorIDsToRevoke_) external onlyOwner {
        _burnBatch(alumnusWallet_, majorIDsToRevoke_, _getAmounts(majorIDsToRevoke_.length));
    }

    function changeWallet(address from_, address to_) external onlyOwner {
        require(alumniData[to_].heldTokenIDs.length == 0, "To wallet used");
        uint256[] memory ids = alumniData[from_].heldTokenIDs;
        require(ids.length > 0, "No majors");

        if (owner() != from_) {
            _setApprovalForAll(from_, owner(), true);
        }
        // use super, since child transfers revert
        super.safeBatchTransferFrom(from_, to_, ids, _getAmounts(ids.length), "");
        alumniData[to_] = alumniData[from_];
        delete alumniData[from_];
    }

    /// @dev Not using standard methods, to avoid confusion as almost all tokens have to be transferred
    function safeTransferFrom(
        address from,
        address to,
        uint256 id,
        uint256 amount,
        bytes memory data
    ) public override {
        revert("Non transferable");
    }

    /// @dev Not using standard methods, to avoid confusion as almost all tokens have to be transferred
    function safeBatchTransferFrom(
        address from,
        address to,
        uint256[] memory ids,
        uint256[] memory amounts,
        bytes memory data
    ) public override {
        revert("Non transferable");
    }

    /// @dev Taken from ERC1155 standard (was private)
    function _getAmounts(uint256 len) private pure returns (uint256[] memory) {
        uint256[] memory amounts = new uint256[](len);
        for (uint256 i = 0; i < len; i++) {
            amounts[i] = 1;
        }
        return amounts;
    }

    function getMetadata(uint256 tokenId) private view returns (string memory) {
        string memory major = _majorByTokenID[tokenId];
        require(bytes(major).length > 0, "ERC1155: URI get of nonexistent token");

        string memory attributes = string(abi.encodePacked(
                '[{"trait_type": "', major, '", "value": "Received"}]'));
        string memory json = Base64.encode(bytes(string(
                abi.encodePacked('{"name": "University Alumnus", "description": "These SBTs are held by University Alumni.", "attributes":',
                attributes, ', "image": "', imgUri, Strings.toString(tokenId), '.png"}')
            )));

        return json;
    }

    /** @dev One img for all, use sth. like img-site.app/img/nft.png?ref=
    * Otherwise img-site.app/img/ which will resolve to img-site.app/img/1.png */
    function setImgUri(string memory imgUri_) external onlyOwner {
        imgUri = imgUri_;
    }

    function uri(uint256 tokenId) public view override returns (string memory) {
        string memory json = getMetadata(tokenId);
        // non-existent token check integrated
        return string(abi.encodePacked('data:application/json;base64,', json));
    }
}
