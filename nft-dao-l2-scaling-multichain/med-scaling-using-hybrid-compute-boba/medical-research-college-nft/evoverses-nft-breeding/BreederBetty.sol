// SPDX-License-Identifier: MIT
pragma solidity 0.8.9;

import "@openzeppelin/contracts-upgradeable/access/AccessControlEnumerableUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/utils/structs/EnumerableMapUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/utils/structs/EnumerableSetUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/security/PausableUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/proxy/utils/Initializable.sol";
import "@openzeppelin/contracts-upgradeable/utils/CountersUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/utils/StringsUpgradeable.sol";
import "@openzeppelin/contracts-upgradeable/utils/Base64Upgradeable.sol";

import "../../ERC721/interfaces/EvoStructs.sol";
import "../../utils/constants/NpcConstants.sol";
import "../../utils/boba/ITuringHelper.sol";
import "../../ERC721/interfaces/IEvo.sol";
import "../HatcherHarry/IHatcherHarry.sol";
import "../../ERC721/EvoEgg/IEvoEgg.sol";
import "./BBFeeDistributor.sol";

/**
* @title Evo v1.0.0
* @author @DirtyCajunRice
*/
contract BreederBetty is Initializable, EvoStructs, PausableUpgradeable,
AccessControlEnumerableUpgradeable, BBFeeDistributor, NpcConstants {

    using StringsUpgradeable for uint256;

    ITuringHelper private _turing;
    IEvo private _Evo;
    IEvoEgg private _EvoEgg;
    IHatcherHarry private _hatcherHarry;

    event EvoBred(address indexed from, uint256 cost, uint256 parent1, uint256 parent2, Egg evoEgg);
    /// @custom:oz-upgrades-unsafe-allow constructor
    constructor() {
        _disableInitializers();
    }

    function initialize() initializer public {

        __Pausable_init();
        __AccessControlEnumerable_init();

        __BBFeeDistributor_init(0xc8849f32138de93F6097199C5721a9EfD91ceE01);

        _grantRole(DEFAULT_ADMIN_ROLE, _msgSender());
        _grantRole(ADMIN_ROLE, _msgSender());
        _grantRole(CONTRACT_ROLE, _msgSender());

        _turing = ITuringHelper(0x680e176b2bbdB2336063d0C82961BDB7a52CF13c);
        _Evo = IEvo(0x3e9694a37846C864C67253af6F5d1F534ff3BF46);
        _EvoEgg = IEvoEgg(0xa3b63C50F0518aAaCf5cF4720B773e1371D10eBF);
        _hatcherHarry = IHatcherHarry(0x918eA0E87ef08a5931aD3777b8c0EB69e2Ce37Dd);
    }

    function pause() public onlyRole(ADMIN_ROLE) {
        _pause();
    }

    function unpause() public onlyRole(ADMIN_ROLE) {
        _unpause();
    }

    function breed(uint256[] memory tokenIds) external {
        require(tokenIds.length == 2, "Invalid tokenId count");
        require(tokenIds[0] != tokenIds[1], "Cant breed with self");
        uint256 totalFee = 0;
        Evo[] memory evos = new Evo[](2);
        for (uint256 i = 0; i < tokenIds.length; i++) {
            require(_Evo.ownerOf(tokenIds[i]) == _msgSender(), "Not owner");
            Evo memory evo = _Evo.getEvo(tokenIds[i]);
            require(checkBreedCount(evo), "Evo has no breeds remaining");
            require(checkBreedTime(evo), "Evo is still recovering");
            evos[i] = evo;
            totalFee += breedCostOf(evo);
        }
        require(checkCompatibility(evos), "Evo are not compatible");

        _distributeFee(totalFee);

        Egg memory egg = _breed(evos);
        _EvoEgg.incubate(_msgSender(), egg);
        emit EvoBred(msg.sender, totalFee, tokenIds[0], tokenIds[1], egg);
    }

    function _breed(Evo[] memory evos) internal returns(Egg memory) {
        _Evo.addToAttribute(evos[0].tokenId, 6, 1);
        _Evo.addToAttribute(evos[1].tokenId, 6, 1);

        uint256 rand = _turing.Random();
        uint256 weightedOutcome = rand % 1000;
        uint256 speciesId = 0;
        // 50% chance if matching parent species
        if (evos[0].species == evos[1].species && weightedOutcome <= 500) {
            speciesId == evos[0].species;
        // 12.5% chance otherwise for each parent
        } else if (weightedOutcome < 125) {
            speciesId == evos[0].species;
        } else if (weightedOutcome < 250) {
            speciesId == evos[1].species;
        } else {
            speciesId = _hatcherHarry.speciesRoll(rand);
        }
        return Egg(0, speciesId, getChildGeneration(evos), evos[0].tokenId, evos[1].tokenId, 0, block.timestamp);
    }


    function getChildGeneration(Evo[] memory evos) internal pure returns(uint256) {
        uint256 generation = evos[0].generation;
        if (evos[1].generation > generation) {
            generation = evos[1].generation;
        }
        return generation + 1;
    }

    function checkBreedCount(Evo memory evo) internal pure returns(bool) {
        uint256 maxBreedCount = 5;
        if (evo.generation == 0) {
            return true;
        }
        return evo.breeds.total < maxBreedCount;
    }

    function checkBreedTime(Evo memory evo) internal view returns(bool) {
        uint256 baseTime = 7 days;
        uint256 breedTime = 1 days;
        if (evo.generation < 6) {
            breedTime = baseTime - (evo.generation * 1 days);
        }
        uint256 nextBreedTime = evo.breeds.lastBreedTime + breedTime;
        return block.timestamp >= nextBreedTime;
    }

    function checkCompatibility(Evo[] memory evos) internal pure returns(bool) {
        Attributes memory a = evos[0].attributes;
        Attributes memory b = evos[1].attributes;
        if (a.primaryType == b.primaryType) {
            return true;
        }
        if (a.secondaryType > 0 && a.secondaryType == b.primaryType) {
            return true;
        }
        if (b.secondaryType > 0 && b.secondaryType == a.primaryType) {
            return true;
        }
        return false;
    }

    function breedCostOf(Evo memory evo) internal pure returns(uint256) {
        uint256 baseCost = 250 ether;
        uint256 generationBaseCost = baseCost * (2**evo.generation);
        uint256 totalBreeds = evo.breeds.total;
        if (evo.generation == 0 && totalBreeds > 4) {
            totalBreeds = 4;
        }
        return generationBaseCost + (generationBaseCost * totalBreeds);
    }
}
