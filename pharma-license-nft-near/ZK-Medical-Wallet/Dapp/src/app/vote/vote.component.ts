import { Component, OnInit } from "@angular/core";
import { HttpClient } from "@angular/common/http";
import { MatSnackBar } from "@angular/material";
import { Web3Service } from "../util/web3.service";
import Web3 from "web3";
import { Router } from "@angular/router";
// const Election = artifacts.require("Election");
import election_artifact from "../../../build/contracts/Election.json";
// const network_config = {
//     // httpradar: new http("https://api.radarrelay.com/0x/v2"),
//     RPC_PROVIDER: "https://mainnet.infura.io/v3/425313c6627e43ddb43324a9419c9508",
//     NETWORK_ID: 1,
//     ASSET_URL: "https://api.radarrelay.com/v2/markets/",
//     ETHERSCAN_TX: "https://etherscan.io/tx/"
// }
const network_config = {
  // httpradar: new http("https://api.radarrelay.com/0x/v2"),
  RPC_PROVIDER: "http://localhost:8545/",
  NETWORK_ID: 1
};

// setting provider to infura
const web3 = new Web3(
  new Web3.providers.HttpProvider(network_config.RPC_PROVIDER)
);

@Component({
  selector: "app-vote",
  templateUrl: "./vote.component.html",
  styleUrls: ["./vote.component.css"]
})
export class VoteComponent {
  ElectionInstance: any;
  election:any;
  user = { verification_status: true };
  // user : any;
  model = {
    uuid: null,
    constituency: "",
    candidates: null,
    show_voting_info: false,
    selected_party: "",
    security_token: "",
    accounts: null,
    primary_account: null,
    vote_hash: "",
    has_voted: false
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
    this.watchAccount();
    this.model.accounts = await web3.eth.getAccounts();
    console.log(this.model.accounts);
    this.model.primary_account = this.model.accounts[0];
    this.model.uuid = this.web3Service.uuid;
    this.user = { verification_status: false };
    // this.model.uuid =22;

    this.web3Service
      .artifactsToContract(election_artifact)
      .then(ElectionAbstraction => {
        this.ElectionInstance = ElectionAbstraction;
        this.ElectionInstance.deployed().then(deployed => {
          console.log(deployed);
          this.ElectionInstance = deployed;

          console.log("calling get info");
          this.get_info();
        });
      });
      

      await this.getContracts();
      this.model.accounts = await web3.eth.getAccounts();
      this.model.primary_account = this.model.accounts[0];
      this.get_info();
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
    let chainId = await web3.eth.net.getId();
    this.election = new web3.eth.Contract(election_artifact.abi as any, election_artifact["networks"][chainId.toString()]["address"]);
    console.log(this.election);
}

  get_info() {
    let url = "/v1/kyc/info/" + this.model.uuid.toString() + "/";
    console.log("inside get info ", url);
    this.http.get(url).subscribe(
      async (res: any) => {
        console.log(res);
        this.user = res;
        let has_voted = await<any> this.check_has_voted();
        if (has_voted){
            return;
        }
        this.model.constituency = res["constituency"];

        if (this.model.constituency != null) {
          let url =
            "/v1/constituency/" + this.model.constituency + "/candidate_list/";
          this.http.get(url).subscribe(
            res => {
              console.log(res);
              this.model.candidates = res;
              this.model.show_voting_info = true;
            },
            error => {
              console.log(error);
            }
          );
        } else {
          this.setStatus(
            "Cant fetch voting information correctly. Check after some time"
          );
        }
      },
      error => {
        console.log(error);
      }
    );
  }

  async vote() {
    let vote_hash = web3.utils.soliditySha3(
      this.model.uuid.toString() + this.model.security_token
    );
    console.log(vote_hash);
    this.model.vote_hash = vote_hash;
    let voter_id_hash = web3.utils.soliditySha3(this.user["voter_id"]);
    console.log(voter_id_hash);
    console.log(
      voter_id_hash,
      " ; ",
      this.model.constituency,
      " : ",
      this.model.selected_party,
      " : ",
      vote_hash
    );
    
    // const nonce = await this.web3Service.getNonce(this.model.primary_account);
    // console.log("Got nonce: ", nonce);
    
    var tx_hash = await this.election.methods.registerVote(voter_id_hash,
      this.model.constituency,
      this.model.selected_party,
      vote_hash).send({
      from:this.model.accounts[0],gas:600000 
    }).on("receipt", receipt => {
      this.setStatus("Verified Successfully!");
      console.log("added");
      console.log(receipt);
    });
    console.log("Tx hash : " , tx_hash);
    this.model.show_voting_info = false;
    this.setStatus("Success! Vote Confirmed and stored on blockchain");


    // this.ElectionInstance.registerVote
    //   .sendTransaction(voter_id_hash,
    //     this.model.constituency,
    //     this.model.selected_party,
    //     vote_hash, {
    //     from: this.model.primary_account,
    //     nonce: nonce
    //   })
    //   .then((res, err) => {
    //     if (err !== undefined) {
    //       console.error(err);
    //     } else {
    //       console.log(res.receipt.status);
    //       if (res.receipt.status === true) {
    //         console.log("block mined");
    //         console.log(res.receipt);
    //         this.model.show_voting_info = false;
    //         this.setStatus(
    //             "Vote Confirmed! You can view the transaction on etherscan"
    //         );
    //       }
    //     }
    //   });

    // let tx_hash = await this.ElectionInstance.registerVote(
    //   voter_id_hash,
    //   this.model.constituency,
    //   this.model.selected_party,
    //   vote_hash,
    //   { from: this.model.accounts[0] }
    // ).on("receipt", receipt => {
    //   console.log("block mined");
    //   console.log(receipt);
    //   this.model.show_voting_info = false;
    //   this.setStatus(
    //     "Vote Confirmed! You can view the transaction on etherscan"
    //   );
    // });

    // console.log(tx_hash);
  }

  setStatus(status) {
    this.matSnackBar.open(status, null, { duration: 3000 });
  }

  setUuid(e) {
    this.model.uuid = e.target.value;
  }

  setSelectedCandidate(e) {
    this.model.selected_party = e.value;
    console.log(this.model.selected_party);
  }

  setSecurityToken(e) {
    this.model.security_token = e.target.value;
    console.log(this.model.security_token);
  }
  async check_has_voted() {
    let voter_id_hash = web3.utils.soliditySha3(this.user["voter_id"]);
    let has_voted = await this.ElectionInstance.hasVoted.call(voter_id_hash);
    console.log("has voted : ", has_voted);
    this.model.has_voted = has_voted;
    return has_voted;
  }

  dashboard() {
    this.router.navigateByUrl("/dashboard");
  }

  home() {
    this.router.navigateByUrl('/home');
  }
}
