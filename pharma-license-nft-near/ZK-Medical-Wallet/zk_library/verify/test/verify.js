
const AgeVerifier = artifacts.require('AgeVerifier');
const PasswordVerifier = artifacts.require('PasswordVerifier');
const Election = artifacts.require('Election');
const age_proof = require("../../age/proof.json");
const password_proof = require("../../password/proof.json");
const Web3 = require('web3')
var web3 = new Web3();
const BigNumber = require('bignumber.js');

// console.log(proof);
contract('AgeVerifier', (accounts) => {
    let AgeVerifierInstance;
    let PasswordVerifierInstance;
    let creator;
    let ElectionInstance;


    before(async () => {
        AgeVerifierInstance = await AgeVerifier.deployed();
        PasswordVerifierInstance = await PasswordVerifier.deployed();
        ElectionInstance = await Election.deployed();
        
    })

    it('should be able to deploy', async () => {

        // assert.equal(dummy, 1, "not deployed correctly");
        // assert.notEqual(CompoundDAIMarketInstance.address, "0x0000000000000000000000000000000000000000");
    });


    
    
    it("should verify age correctly", async () => {

        // console.log(age_proof);
        verification_status = await AgeVerifierInstance.verifyTx.call(age_proof["proof"]["a"],age_proof["proof"]["b"],age_proof["proof"]["c"],age_proof["inputs"], {from:accounts[0], gas: 4000000});
        console.log(verification_status);
        // console.log(verification_status["logs"]);
        
    });
    
    it("should verify password correctly", async () => {

        verification_status = await PasswordVerifierInstance.verifyTx.call(password_proof["proof"]["a"],password_proof["proof"]["b"],password_proof["proof"]["c"],password_proof["inputs"], {from:accounts[0], gas: 4000000});
        console.log(verification_status);
        // console.log(verification_status["logs"]);   
    });

    it("should verify voter eligibility correctly", async () => {

        let password = "secure voting key";
        let commitment_hex = web3.utils.keccak256(password);
        console.log("commitment hash : ", commitment_hex);
        let x1 = BigNumber(commitment_hex.substr(0,8), 16).toFixed();
        console.log("final commitment : ", x1);
        let receipt = await ElectionInstance.addCommitment(x1 , {from:accounts[0], gas: 4000000});
        let receipt1 = await ElectionInstance.addCommitment(x1 , {from:accounts[0], gas: 4000000});
        let receipt2 = await ElectionInstance.addCommitment(x1 , {from:accounts[0], gas: 4000000});
        // console.log(receipt);

        let commitments = await ElectionInstance.getCommitments.call();
        let input_proof = [];
        for(i=0; i<3; i++){
            let x =  new BigNumber(commitments[0]).toFixed();
            input_proof.push(x);
        }
        console.log("commitments : ", new BigNumber(commitments[0]).toFixed());
        console.log("input proof: ", input_proof);
         
    });

    
   
})