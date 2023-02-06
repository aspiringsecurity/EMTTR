import { Signer } from "@ethersproject/abstract-signer";
import { Wallet } from "@ethersproject/wallet";
import { expect } from "chai";
import { ethers, network } from "hardhat";
import { HackathonManager } from "../typechain-types";

describe("HackathonManagerFactory", function () {

    describe("Deployment", function () {
        it("Deployer is Owner of HacatonManagerFactory", async function () {
        
            // fund random address with eth
            const random = Wallet.createRandom();
            await network.provider.send("hardhat_setBalance", [
                random.address,
                "0xfffffffffffffffff",
              ]);
        
            const signer = new ethers.Wallet(random, ethers.provider);
            
            const HackathonManagerFactory = await ethers.getContractFactory("HackathonManagerFactory", signer);
            const hackathonManagerFactory = await HackathonManagerFactory.deploy();
            
            
            expect(await hackathonManagerFactory.Owner()).to.equal(signer.address);
        });

        it("Triggers the HackCreated Event when a hack is created", async function () {
            const HackathonManagerFactory = await ethers.getContractFactory("HackathonManagerFactory");
            const hackathonManagerFactory = await HackathonManagerFactory.deploy();
            
            const hackName = "new hack";

            await expect(hackathonManagerFactory.createNewHack(hackName))
                .to.emit(hackathonManagerFactory,"HackCreated");
        });
            
        it("Reverts when deploymentfee too low", async function() {

            const HackathonManagerFactory = await ethers.getContractFactory("HackathonManagerFactory");
            const hackathonManagerFactory = await HackathonManagerFactory.deploy();
            
            const hackName = "revert hack";

            await hackathonManagerFactory.setFee(11);
            
            await expect(hackathonManagerFactory.createNewHack(hackName, {value: 10}))
                .to.revertedWithoutReason();
        } );
    });

});

describe("HackatonManager", function () {

    let hackatonManagerAddress : string;
    const hackName = "hackatonmanager";

    beforeEach(async function () {
            
            const HackathonManagerFactory = await ethers.getContractFactory("HackathonManagerFactory");
            const hackathonManagerFactory = await HackathonManagerFactory.deploy();
            
            const rc = await (await hackathonManagerFactory.createNewHack(hackName,{value: 1})).wait();
            
            const event = rc.events?.find(event => event.event === 'HackCreated');
            hackatonManagerAddress = event?.args?._contractAddress ?? '';
            
      });

      it('hackatonmanager has a name', async function() {

        const hackatonmanager =(await ethers.getContractFactory("HackathonManager"))
        .attach(hackatonManagerAddress);
        
        console.log('The name was: ' + await hackatonmanager._hackathonName());
        await expect(await hackatonmanager._hackathonName())
            .to.equal(hackName);

      });
});
