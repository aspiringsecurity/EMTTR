import { Component, OnInit } from "@angular/core";
import { HttpClient } from "@angular/common/http";
import { MatSnackBar } from "@angular/material";
import { Router } from "@angular/router";
import { Web3Service } from "../util/web3.service";
import * as uuid from "uuid";
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
  selector: "app-dashboard",
  templateUrl: "./dashboard.component.html",
  styleUrls: ["./dashboard.component.css"]
})
export class DashboardComponent implements OnInit {
  VoterDataInstance: any;
  logged_in = false;

  model = {
    uuid: null,
    password: "",
    accounts: null,
    primary_account: null
  };

  // user = {voter_id:''};
  user : any;


  displayedColumns: string[] = ['Label', 'Start Time', 'End Time', 'Action'];
  

  elections : any;
  voter_data:any;
  // elections = [
  //   {
  //     label: "Test Election",
  //     start_time: "April 21, 2019",
  //     end_time: "May 21, 2019",
  //     status: 0 // 0 - yet to start, 1- ongoing, 2 - completed
  //   }
  // ];

  constructor(
    private matSnackBar: MatSnackBar,
    private http: HttpClient,
    private router: Router,
    private web3Service: Web3Service
  ) {}

  async ngOnInit() {
    console.log("OnInit: " + this.web3Service);
    console.log(this);
    this.watchAccount();
    this.model.accounts = await web3.eth.getAccounts();
    console.log(this.model.accounts);
    this.model.primary_account = this.model.accounts[0];
    this.user = {voter_id:''};
    // this.elections[0] = this.web3Service.election;
    // console.log("election from web3 : " , this.elections[0] );
    this.web3Service
      .artifactsToContract(voterdata_artifact)
      .then((result: any) => {
        this.VoterDataInstance = result;
        this.VoterDataInstance.deployed().then(deployed => {
          console.log(deployed);
          this.VoterDataInstance = deployed;
        });
      });
    
    await this.getContracts();
    this.model.accounts = await web3.eth.getAccounts();
    this.model.primary_account = this.model.accounts[0];

    if (this.web3Service.uuid){
        this.restore(this.web3Service.uuid);
    }
    this.getElections();
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
    console.log("Retrieving contract information...");
    let chainId = await web3.eth.net.getId();
    console.log(voterdata_artifact["abi"]);
    this.voter_data = new web3.eth.Contract(voterdata_artifact["abi"] as any, voterdata_artifact["networks"][chainId.toString()]["address"]);
    console.log(this.voter_data);
  }

  login(uuid: number, pass: string) {

    this.setStatusShort("Logging In ...");
    let url = "/v1/kyc/info/" + uuid.toString() + "/";
    this.http.get(url).subscribe(res => {
      console.log(res);
      this.user = res;
      this.logged_in = true;
      this.web3Service.uuid = this.user.uuid;
      this.model.uuid = this.user.uuid;
      this.setStatusShort("Logged In !");
      this.setEligible();
    });
  }

  restore(uuid: number) {

    let url = "/v1/kyc/info/" + uuid.toString() + "/";
    this.http.get(url).subscribe(res => {
      console.log(res);
      this.user = res;
      this.model.uuid = this.user.uuid;
      this.setEligible();
    });
  }

  setEligible() {
    let dob = this.user.dob;
    let age = (Date.now() - dob) / (1000 * 60 * 60 * 24 * 365);
    this.user["is_eligible"] = age >= 18 ? true : false;
    console.log(this.user);
    console.log(this.user.voter_id == null);
  }

  setStatus(status) {
    this.matSnackBar.open(status, null, { duration: 3000 });
  }

  setStatusShort(status) {
    this.matSnackBar.open(status, null, { duration: 2000 });
  }

  // functions for dashboard
  refreshVerificationStatus() {
    this.setStatus("Refreshing Verification Status ...");
    let url = "/v1/kyc/info/" + this.model.uuid.toString() + "/";
    this.http.get(url).subscribe(res => {
      console.log(res);
      this.user.verification_status = res["verification_status"];
      this.setStatusShort("Status Updated!");
    });
  }

  getVoterId() {
    this.setStatusShort("Generating Voter ID ...");
    const voter_id = uuid.v4().toString();
    // this.user.voter_id = voter_id;
    // let voter_id = 'asf';
    this.updateVoterIdOnBlockchain(this.user.uuid, voter_id);
  }

  updateVoterIdDB(uuid: number, voter_id: string) {
    let data = {
      uuid,
      voter_id
    };
    console.log(data);
    let url = "/v1/kyc/info/voter_id/";
    this.http.post(url, data).subscribe(res => {
      console.log(res);
      this.user.voter_id = voter_id;
    });
  }

  vote() {
    this.setStatusShort("Redirecting to Voting Page ...");
    this.router.navigateByUrl("/vote");
    // this.router.navigateByUrl('/otp_verification');
  }

  getResults(label: string) {
    this.setStatusShort("Redirecting to Results Page ...");
    this.web3Service.setElectionLabel(label);
    this.router.navigateByUrl("/resultsverify");
  }

  async updateVoterIdOnBlockchain(uuid, voter_id: string) {
    let current_time = new Date().getTime();
    let uuidHash = web3.utils.soliditySha3(uuid);

    // Get the nonce & post data to the blockchain
    // const nonce = await this.web3Service.getNonce(this.model.primary_account);
    // console.log("Got nonce: ", nonce);

    var tx_hash = await this.voter_data.methods.generateVoterId(uuidHash, voter_id, current_time).send({
      from:this.model.accounts[0],gas:600000 
    }).on("receipt", receipt => {
      this.updateVoterIdDB(this.user.uuid, voter_id);
      this.setStatus("Success! Voter Id generated and stored on the blockchain");
      console.log("added");
      console.log(receipt);
    });
    console.log("Tx hash : " , tx_hash);

    // Using metamask and truffle contracts 

    // this.VoterDataInstance.generateVoterId
    //   .sendTransaction(uuidHash, voter_id, current_time, {
    //     from: this.model.primary_account,
    //     nonce: nonce
    //   })
    //   .then((res, err) => {
    //     if (err !== undefined) {
    //       console.error(err);
    //     } else {
    //       console.log(res.receipt.status);
    //       if (res.receipt.status === true) {
    //         this.updateVoterIdDB(this.user.uuid, voter_id);
    //       }
    //     }
    //   });
  }

  logout() {
      
      this.logged_in = false;
      this.web3Service.uuid = null;
      this.model.uuid = null;
      console.log("Logged Out.");
      this.setStatusShort("Logged out Successfully!")
      this.router.navigateByUrl("/dashboard");
      
  }

  refreshElections(){
    console.log("refreshing elections..");
    this.getElections();
  }

  getElections() {
    let url = "/v1/elections/";
    console.log("inside get elections ", url);
    this.http.get(url).subscribe(
      res => {
        console.log(res);
        this.elections = res;
      },
      error => {
        console.log(error);
      }
    );
  }

  home() {
    this.router.navigateByUrl('/home');
}
}
