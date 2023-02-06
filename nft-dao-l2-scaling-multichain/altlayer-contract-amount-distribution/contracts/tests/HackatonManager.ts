import { Signer } from "@ethersproject/abstract-signer";
import { Wallet } from "@ethersproject/wallet";
import { expect } from "chai";
import { ethers, network } from "hardhat";
import { HackathonManager } from "../typechain-types";

describe("HackathonManager", function () {

    let hackatonManager : HackathonManager;
    let hackatonOwner : any;
    const trackName = "Super Winner";
    
    beforeEach(async function() {
        
        [hackatonOwner] = await ethers.getSigners();
        const hackatonName = "name";
        const HackathonManager = await ethers.getContractFactory("HackathonManager");
        
        hackatonManager = await HackathonManager.deploy(hackatonOwner.address, hackatonName);
        
        await hackatonManager.fundHackathon({value: 200});
        
    });
    
    it("CreateTrack emits TrackCreated", async function() {
        
        
        await expect(hackatonManager.createTrack(trackName, 1))
        .to.emit(hackatonManager,"TrackCreated");
    });
    
    it ("Prizes can be added to a 'Track' after funded", async function() {

        const prizeName = "1 million dollar bounty";
        const tx = await hackatonManager.createTrack(trackName, 100);
        await tx.wait();

        await expect( hackatonManager.addPrizeToTrack(trackName, prizeName, 1))
            .to.emit(hackatonManager, "PrizeAddedToTrack")
            .withArgs(trackName, prizeName,1);

        
    });



    it ("Participants can register ", async function () {

        const teamname = "Team 9";
        const projectname = "Wen bounty?";
        const projectlink = "https://localhost:3000";

        const stateBefore =await  hackatonManager._state();
        expect(stateBefore).to.equal(0);

        await expect(hackatonManager.registerParticipant(teamname, projectname, projectlink))
            .to.emit(hackatonManager, "ParticipantRegistered");

    
    });


    it ("Should be able to get state of hackaton", async function() {

        const state =await  hackatonManager._state();
        console.log("State: " + state);
    });


    it ("Registered participants can submit project", async function() {
        const teamname = "Team 9";
        const projectname = "Wen bounty?";
        const projectlink = "https://localhost:3000";

    
        await expect(hackatonManager.registerParticipant(teamname, projectname, projectlink))
            .to.emit(hackatonManager, "ParticipantRegistered");

        await expect( hackatonManager.submitProject(teamname))
            .to.emit(hackatonManager, "ProjectSubmitted")
            .withArgs(teamname, projectlink);

    });

    it("Capture winner emits ['prize paid']", async () => {


        // create track with a 1 million dollar bounty!
        const prizeName = "1 million dollar bounty";
        const prizeAmount = 5;
        const tx = await hackatonManager.createTrack(trackName, 100);
        await tx.wait();

        // add prize to track
        await (await hackatonManager.addPrizeToTrack(trackName, prizeName, prizeAmount)).wait();
            

        
        // participant registers
        const teamname = "Team 9";
        const projectname = "Wen bounty?";
        const projectlink = "https://localhost:3000";

        await (await hackatonManager.registerParticipant(teamname, projectname, projectlink)).wait();
            


        // participant submits project
        await (await hackatonManager.submitProject(teamname)).wait();


       // capture winner
        await expect(hackatonManager.captureWinner(trackName,prizeName, teamname))
            .to.emit(hackatonManager,"PrizePaid" )
            .to.changeEtherBalance(hackatonOwner.address, prizeAmount);

        
    });

});
