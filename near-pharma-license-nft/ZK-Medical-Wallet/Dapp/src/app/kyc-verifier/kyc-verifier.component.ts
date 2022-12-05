import { Component, OnInit } from "@angular/core";
import { HttpClient } from "@angular/common/http";
import { MatSnackBar } from "@angular/material";
import { Router } from "@angular/router";
import { Web3Service } from "../util/web3.service";
import Web3 from "web3";
import voterdata_artifact from "../../../build/contracts/VoterData.json";
import moment from "moment";

const network_config = {
  RPC_PROVIDER: "http://localhost:8545/",
  NETWORK_ID: 1
};

const web3 = new Web3(
  new Web3.providers.HttpProvider(network_config.RPC_PROVIDER)
);

@Component({
  selector: "app-kyc-verifier",
  templateUrl: "./kyc-verifier.component.html",
  styleUrls: ["./kyc-verifier.component.css"]
})
export class KycVerifierComponent implements OnInit {
  VoterDataInstance: any;
  voter_data:any;
  // Dummy verifier (in actual practice, login mechanism would be used)
  verifier = {
    name: "Amit Singh"
  };

  // unverifiedVoters = UNVERIFIED_VOTERS;
  unverifiedVoters: any;

  displayedColumns: string[] = [
    "Name",
    "UUID",
    "Date of Birth",
    "Constituency",
    "Status"
  ];

  model = {
    accounts: null,
    primary_account: null
  };

  constructor(
    private matSnackBar: MatSnackBar,
    private http: HttpClient,
    private router: Router,
    private web3Service: Web3Service
  ) {}

  async ngOnInit() {
    console.log("OnInit: " + this.web3Service);
    console.log(this);
    this.getVoterList();
    this.watchAccount();
    this.model.accounts = await web3.eth.getAccounts();
    console.log(this.model.accounts);
    this.model.primary_account = this.model.accounts[0];

    this.web3Service
      .artifactsToContract(voterdata_artifact)
      .then((result: any) => {
        this.VoterDataInstance = result;
        this.VoterDataInstance.deployed().then(deployed => {
          console.log("deployed contract : ", deployed);
          this.VoterDataInstance = deployed;
        });
      });

    await this.getContracts();
    this.model.accounts = await web3.eth.getAccounts();
    this.model.primary_account = this.model.accounts[0];
  }

  watchAccount() {
    this.web3Service.accountsObservable.subscribe(accounts => {
      this.model.accounts = accounts;
      this.model.primary_account = accounts[0];
      console.log(accounts[0]);
      // this.refreshBalance();
    });
  }

  async getContracts() {
    console.log("Retrieving contract information...")
    let chainId = await web3.eth.net.getId()
    this.voter_data = new web3.eth.Contract(voterdata_artifact.abi as any, voterdata_artifact["networks"][chainId.toString()]["address"]);
    console.log(this.voter_data);
}

  setStatus(status) {
    this.matSnackBar.open(status, null, { duration: 3000 });
  }

  setStatusShort(status) {
    this.matSnackBar.open(status, null, { duration: 2000 });
  }

  public async verify(voter: any) {
    this.setStatus("Verifying " + voter.name);
    // Handle dates using moments. Not required anymore
    // let voter_dob = moment(new Date(voter.dob).toUTCString()).valueOf() / 1000;
    // let current_time = moment(new Date().toUTCString()).valueOf() / 1000;

    let voter_dob = new Date(voter.dob).getTime();
    let current_time = new Date().getTime();
    let uuidHash = web3.utils.soliditySha3(voter.uuid);
    
    try{
      // Get the nonce & post data to the blockchain
      // const nonce  = await this.web3Service.getNonce(this.model.primary_account);
      // console.log("Got nonce: ", nonce);
      console.log("account from : ", this.model.primary_account);

      var tx_hash = await this.voter_data.methods.kycVerify(uuidHash, voter.name, voter_dob, current_time).send({
        from:this.model.accounts[0],gas:600000 
      }).on("receipt", receipt => {
        this.updateVerificationStatusDB(voter.name, voter.uuid);
        this.setStatus("Verified Successfully!");
        console.log("added");
        console.log(receipt);
      });
      console.log("Tx hash : " , tx_hash);

      // this.VoterDataInstance.kycVerify.sendTransaction(uuidHash, voter.name, voter_dob, current_time, {from: this.model.primary_account, nonce: nonce})
      //   .then((res, err) => {
      //     if(err !== undefined){
      //       console.error("Error!!!!", err);
      //       this.setStatus("Error: Unable to verify! Please try again later");
      //       // voter.verification_status = "unverified";
      //     }
      //     else{
      //       console.log(res.receipt.status);
      //       if(res.receipt.status == true){
      //         console.log("receipt : ", res.receipt);
      //         this.updateVerificationStatusDB(voter.name, voter.uuid);
      //       }
      //       else{
      //         console.log("transaction failed. check receipt : ", res.receipt);
      //         this.setStatus("Error: Unable to verify! Please try again later");

      //       }

      //     }
      //   }
      // );
    }
    catch(err){
      console.log("Error!!", err);
      this.setStatus("Error: Unable to verify! Please try again later");
    }

    // this.updateVerificationStatusDB(voter.name, voter.uuid);
  }

  async updateVerificationStatusDB(name: string, uuid: number) {
    var data = {
      uuid
    };
    var url = "/v1/kyc/info/verify/";
    this.http.post(url, data).subscribe(res => {
      console.log(res);
      this.setStatus("Voter " + name + " verified!");
      console.log("calling voter list function inside....");
      this.getVoterList();
    });
  }
  removeVoterFromUnregistered(voter: any) {
    console.log("I am here");
    this.unverifiedVoters.splice(
      this.unverifiedVoters.findIndex(_voter => _voter.uuid == voter.uuid),
      1
    );
    console.log(this.unverifiedVoters);
  }

  getVoterList() {
    console.log("getting the list of voters ...");
    let url = "/v1/kyc/info/list/";
    this.http.get(url).subscribe(res => {
      console.log(res);
      this.unverifiedVoters = res;
    });
  }

  home() {
    this.router.navigateByUrl('/home');
  }
}
