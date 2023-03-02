import { Component, OnInit } from "@angular/core";
import { HttpClient } from "@angular/common/http";
import { MatSnackBar } from "@angular/material";
import { Router } from "@angular/router";
import { Web3Service } from "../util/web3.service";
import * as uuid from "uuid";
import Web3 from 'web3';
import voterdata_artifact from '../../../build/contracts/VoterData.json';
import moment from 'moment';

const network_config = {
  // httpradar: new http("https://api.radarrelay.com/0x/v2"),
  RPC_PROVIDER: "/bc//",
  NETWORK_ID: 1
};

const web3 = new Web3(new Web3.providers.HttpProvider(network_config.RPC_PROVIDER));

@Component({
  selector: "app-user-login",
  templateUrl: "./user-login.component.html",
  styleUrls: ["./user-login.component.css"]
})
export class UserLoginComponent implements OnInit {

  VoterDataInstance: any;

  model = {
    uuid: "",
    password: "",
    accounts: null,
    primary_account: null
  };

  user = {
    uuid: 1231231231321,
    name: "Akash",
    constituency: "Banglore",
    dob: "December 17 1995",
    address: "h9/364, IIT Bombay, Powai",
    voterId: "",
    verification_status: true,
    is_eligible: true
  };

  elections = [
    {
      label: "Test Election",
      startTime: "April 21, 2019",
      endTime: "May 21, 2019",
      status: 0 // 0 - yet to start, 1- ongoing, 2 - completed
    },
    {
      label: "XYZ Election",
      startTime: "April 21, 2019",
      endTime: "June 21, 2019",
      status: 1 
    },
    {
      label: "ABC Election",
      startTime: "September 21, 2019",
      endTime: "October 21, 2019",
      status: 2
    }
  ];

  constructor(
    private matSnackBar: MatSnackBar,
    private http: HttpClient,
    private router: Router,
    private web3Service: Web3Service
  ) {}

  async ngOnInit() {
    console.log('OnInit: ' + this.web3Service);
    console.log(this);
    this.watchAccount();
    this.model.accounts = await web3.eth.getAccounts();
    console.log(this.model.accounts);
    this.model.primary_account = this.model.accounts[0];

    this.web3Service.artifactsToContract(voterdata_artifact)
        .then((result: any) => {
            this.VoterDataInstance = result;
            this.VoterDataInstance.deployed().then(deployed => {
                console.log(deployed);
                this.VoterDataInstance = deployed;
            });

        });
  }

  watchAccount() {
    this.web3Service.accountsObservable.subscribe((accounts) => {
        this.model.accounts = accounts;
        this.model.primary_account = accounts[0];
        console.log(accounts[0])
        // this.refreshBalance();
    });
  }

  verify_user(a1, a2) {
    this.setStatusShort("Verifying User ...");
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
  }

  getVoterId() {
    this.setStatusShort("Generating Voter ID ...");
    const voterId = uuid.v4().toString();
    // let voterId = 'asf';
    this.updateVoterIdOnBlockchain(this.user.uuid, voterId);
  }

  vote() {
    this.setStatusShort("Redirecting to Voting Page ...");
    this.router.navigateByUrl('/login');
  }

  getResults() {
    this.setStatusShort("Redirecting to Results Page ...");
    this.router.navigateByUrl('/resultsverify');
  }

  async updateVoterIdOnBlockchain(uuid, voterId: string){
    let current_time = moment(new Date().toUTCString()).valueOf() / 1000;
    let uuidHash = web3.utils.soliditySha3(uuid);

    // Get the nonce & post data to the blockchain
    const nonce  = await this.web3Service.getNonce(this.model.primary_account);
    console.log("Got nonce: ", nonce);
    this.VoterDataInstance.generateVoterId.sendTransaction(uuidHash, voterId, current_time, {from: this.model.primary_account, nonce: nonce})
      .then((res, err) => {
        if(err !== undefined){
          console.error(err);
        }
        else{
          console.log(res.receipt.status);
          if(res.receipt.status === true){
            this.user.voterId = voterId;
          }
        }
      }
    );
  }
}
