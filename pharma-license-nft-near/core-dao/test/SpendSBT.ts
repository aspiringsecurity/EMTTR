import { loadFixture } from "@nomicfoundation/hardhat-network-helpers";
import { expect } from "chai";
import { ethers } from "hardhat";

describe("SpendSBT", function () {
  async function deploySpendSBTFixture() {
    const [owner, otherAccount] = await ethers.getSigners();
    const SpendSBT = await ethers.getContractFactory("SpendSBT");
    const spendSBT = await SpendSBT.deploy();
    return { spendSBT, owner, otherAccount };
  }

  describe("Deployment", function () {
    it("Should set the right owner", async function () {
      const { spendSBT, owner } = await loadFixture(deploySpendSBTFixture);
      expect(await spendSBT.owner()).to.equal(owner.address);
    });
  });

  describe("Mint", function () {
    it("Should mint a token", async function () {
      const { spendSBT, owner } = await loadFixture(deploySpendSBTFixture);
      await spendSBT.mintLitSBT("https://abc", "secret_desc", "secret_key");

      expect(await spendSBT.balanceOf(owner.address)).to.equal(1);

      await spendSBT.fetchNfts().then((nfts) => {
        expect(JSON.stringify(nfts[0])).to.equal('["https://abc","secret_desc","secret_key"]');
      });
    });
  });

  describe("Transfer", function () {
    it("Should NOT transfer a token", async function () {
      const { spendSBT, owner, otherAccount } = await loadFixture(deploySpendSBTFixture);
      await spendSBT.mintLitSBT("https://abc", "secret_desc", "secret_key");

      await expect(spendSBT.transferFrom(owner.address, otherAccount.address, 1)).to.be.revertedWith("This a Soulbound token. It cannot be transferred. It can only be burned by the token owner.");
    });
  });

  describe("Burn", function () {
    it("Should burn a token", async function () {
      const { spendSBT, owner } = await loadFixture(deploySpendSBTFixture);
      await spendSBT.mintLitSBT("https://abc", "secret_desc", "secret_key");
      await spendSBT.burn(1);
      expect(await spendSBT.balanceOf(owner.address)).to.equal(0);
    });
  });

  describe("Fetch holders", function () {
    it("Should fetch holders", async function () {
      const { spendSBT, owner } = await loadFixture(deploySpendSBTFixture);
      await spendSBT.mintLitSBT("https://abc", "secret_desc", "secret_key");
      await spendSBT.fetchHolders().then((holders) => {
        expect(holders[0]).to.equal(owner.address);
      });
    });
  });
});
