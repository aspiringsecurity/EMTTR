// SPDX-License-Identifier: MIT

pragma solidity ^0.8.9;

import "@openzeppelin/contracts/token/ERC721/ERC721.sol";
import "@openzeppelin/contracts/interfaces/IERC20.sol";
import "@openzeppelin/contracts/interfaces/IERC2981.sol";
import "@openzeppelin/contracts/access/Ownable.sol";
import "@openzeppelin/contracts/security/ReentrancyGuard.sol";
import "@openzeppelin/contracts/utils/Counters.sol";
import "@openzeppelin/contracts/utils/Address.sol";

/*
......................................................................................................
.##........#######...######..########......####.......########..#######..##.....##.##....##.########..
.##.......##.....##.##....##....##........##..##......##.......##.....##.##.....##.###...##.##.....##.
.##.......##.....##.##..........##.........####.......##.......##.....##.##.....##.####..##.##.....##.
.##.......##.....##..######.....##........####........######...##.....##.##.....##.##.##.##.##.....##.
.##.......##.....##.......##....##.......##..##.##....##.......##.....##.##.....##.##..####.##.....##.
.##.......##.....##.##....##....##.......##...##......##.......##.....##.##.....##.##...###.##.....##.
.########..#######...######.....##........####..##....##........#######...#######..##....##.########..
......................................................................................................
..........................................VOLUME 1 - RELOADED.........................................
......................................................................................................
.........................A COLLABORATION BETWEEN DANNY DIAMONDS & TRANQUI.ETH.........................                             
......................................................................................................
*/

contract lostandfound_vol_1_reloaded is ERC721, IERC2981, ReentrancyGuard, Ownable {
   using Counters for Counters.Counter;
   
   constructor(string memory customBaseURI_) ERC721("Lost & Found, Vol. 1 - Reloaded", "LF") {
      customBaseURI = customBaseURI_;
   }

   // ========== MINTING LIMITS ==========

   mapping(address => uint256) private mintCountMap;
   
   mapping(address => uint256) private allowedMintCountMap;
   
   uint256 public constant MINT_LIMIT_PER_WALLET = 2;
   
   function allowedMintCount(address minter) public view returns (uint256) {
      return MINT_LIMIT_PER_WALLET - mintCountMap[minter];
   }

   function updateMintCount(address minter, uint256 count) private {
      mintCountMap[minter] += count;
   }

   // ========== MINTING FUNCTIONALITY ==========

   uint256 public constant MAX_SUPPLY = 12;

   uint256 public constant MAX_MULTIMINT = 2;

   uint256 public constant PRICE = 100000000000000000;

   Counters.Counter private supplyCounter;

   function mint(uint256 count) public payable nonReentrant {
      if (allowedMintCount(msg.sender) >= count) {
         updateMintCount(msg.sender, count);
      }  else {
         revert("Minting limit exceeded");
      }

      require(totalSupply() + count - 1 < MAX_SUPPLY, "Exceeds max supply");

      require(count <= MAX_MULTIMINT, "Mint at most 2 at a time");

      require(
         msg.value >= PRICE * count, "Insufficient payment, 0.1 ETH per item"
      );

      for (uint256 i = 0; i < count; i++) {
         _mint(msg.sender, totalSupply());

         supplyCounter.increment();
      }
   }

   function totalSupply() public view returns (uint256) {
      return supplyCounter.current();
   }

   // ========== URI FUNCTIONALITY ==========

   string private customContractURI =
      "ipfs:///bafybeigvd2omjwbdn5nw4sf3xsvyzafl3ebwcwtd6zjagdiwa45ozwhhhe";

   function setContractURI(string memory customContractURI_) external onlyOwner {
      customContractURI = customContractURI_;
   }

   function contractURI() public view returns (string memory) {
      return customContractURI;
   }

   string private customBaseURI;

   function setBaseURI(string memory customBaseURI_) external onlyOwner {
      customBaseURI = customBaseURI_;
   }

   function _baseURI() internal view virtual override returns (string memory) {
      return customBaseURI;
   }

   function tokenURI(uint256 tokenId) public view override
      returns (string memory)
   {
      return string(abi.encodePacked(super.tokenURI(tokenId), ".token.json "));
   }

   // ========== PAYOUT FUNCTIONALITY ==========

   address private constant payoutAddress1 =
      0x45bB50734c961b1629b5Bb70A4a7a3b3d3C4852a;

   address private constant payoutAddress2 =
      0x806164c929Ad3A6f4bd70c2370b3Ef36c64dEaa8;

   function withdraw() public nonReentrant {
      uint256 balance = address(this).balance;

      Address.sendValue(payable(payoutAddress1), balance * 50 / 100);

      Address.sendValue(payable(payoutAddress2), balance * 50 / 100);
   }

   function withdrawTokens(IERC20 token) public onlyOwner {
      uint256 balance = token.balanceOf(address(this));

      token.transfer(payoutAddress1, balance * 50 / 100);

      token.transfer(payoutAddress2, balance * 50 / 100);
   }   

   // ========== ROYALTY FUNCTIONALITY ==========

   function royaltyInfo(uint256, uint256 salePrice) external view override
      returns (address receiver, uint256 royaltyAmount)
   {
    return (address(this), (salePrice * 1500) / 10000);
   }

   function supportsInterface(bytes4 interfaceId)
      public
      view
      virtual
      override(ERC721, IERC165)
      returns (bool)
   {
      return (
         interfaceId == type(IERC2981).interfaceId ||
         super.supportsInterface(interfaceId)
      );
   }

   receive() external payable {}
}

// Contract inspired by Studio 721 & Crypto Coven