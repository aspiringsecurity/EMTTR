// SPDX-License-Identifier: MIT
pragma solidity ^0.8.4;

import "@openzeppelin/contracts/token/ERC721/extensions/ERC721URIStorage.sol";
import "@openzeppelin/contracts/utils/Base64.sol";
import "@openzeppelin/contracts/access/Ownable.sol";

contract GiveawayNFT is ERC721URIStorage, Ownable {
  uint256 tokenId;

  event Minted(address owner, uint256 tokenId, string tokenUri);

  constructor() ERC721("#Giveaway", "GIV") {}

  function mint(address to) public onlyOwner {
    string memory uri = Base64.encode(
      bytes(
        string(
          abi.encodePacked(
            '{"name": "#Giveaway", "image": "ipfs://bafkreiezf2gxxkxiwaeds4fcr2fttme5ilanxyymsvgmvmz6t7q6lnihkq"}'
          )
        )
      )
    );

    string memory tokenUri = string(abi.encodePacked("data:application/json;base64,", uri));

    unchecked {
      tokenId++;
    }

    _safeMint(to, tokenId);
    _setTokenURI(tokenId, tokenUri);

    emit Minted(to, tokenId, tokenUri);
  }
}
