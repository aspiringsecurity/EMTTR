import { Component, OnInit } from "@angular/core";
import { ActivatedRoute } from "@angular/router";
import { HttpClient } from "@angular/common/http";
import { MatSnackBar } from "@angular/material";
import { Web3Service } from "../util/web3.service";
import { Router } from '@angular/router';
import Web3 from "web3";

import election_artifact from "../../../build/contracts/Election.json";

var network_config = {
  // httpradar: new http("https://api.radarrelay.com/0x/v2"),
  RPC_PROVIDER: "http://localhost:8545/",
  NETWORK_ID: 1
};

var web3 = new Web3(
  new Web3.providers.HttpProvider(network_config.RPC_PROVIDER)
);

@Component({
  selector: "app-election",
  templateUrl: "./election.component.html",
  styleUrls: ["./election.component.css"]
})
export class ElectionComponent implements OnInit {
  election_id = 0;
  data = "adsf";
  elections : any;
  election: {
    id: 1,
    label: "Test Election",
    start_time: "August 21, 2019",
    end_time: "August 31, 2019",
    status: 2
};
  ElectionInstance: any;
  electionInstance:any;

  model = {
    accounts: null,
    primary_account: null
  };

  phases = [
    {
      name:'Election yet to start',
      complete:true,
      status:0
    },
    {
      name:'Election Running',
      complete:true,
      status:1
    },
    {
      name:'Election Ended',
      complete:false,
      status:2
    },
    {
      name:'Results declared',
      complete:false,
      status:3
    },
  ]

  constructor(private route: ActivatedRoute, private matSnackBar: MatSnackBar,
    private http: HttpClient,
    private web3Service: Web3Service,
    private router: Router) {
    this.route.params.subscribe(params => {
      console.log(params.id);
      this.election_id = params.id;
    });
  }

  async ngOnInit() {
    console.log("OnInit: " + this.web3Service);
    console.log(this);
    this.watchAccount();
    this.model.accounts = await web3.eth.getAccounts();
    console.log(this.model.accounts);
    // this.elections[0] = this.web3Service.election;
    // console.log("election from web3 : ", this.elections[0]);
    // console.log("new election : ", this.elections);

    // this.web3Service
    //   .artifactsToContract(election_artifact)
    //   .then(ElectionAbstraction => {
    //     this.ElectionInstance = ElectionAbstraction;
    //     this.ElectionInstance.deployed().then(deployed => {
    //       console.log(deployed);
    //       this.ElectionInstance = deployed;
    //     });
    //   });
      
      await this.getContracts();
      this.model.accounts = await web3.eth.getAccounts();
      this.model.primary_account = this.model.accounts[0];
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
    console.log("Retrieving contract information...")
    let chainId = await web3.eth.net.getId()
    this.electionInstance = new web3.eth.Contract(election_artifact.abi as any, election_artifact["networks"][chainId.toString()]["address"]);
    console.log("Here is the contract object", this.electionInstance);
}

  setStatus(status) {
    this.matSnackBar.open(status, null, { duration: 3000 });
  }
  setStatusShort(status) {
    this.matSnackBar.open(status, null, { duration: 2000 });
  }

  async calculateVotes() {
    // var tx_hash = await this.ElectionInstance.calculateVotes({
    //   from: this.model.accounts[0]
    // }).on("receipt", receipt => {
    //   console.log("Calculated");
    //   console.log(receipt);
    // });

    // console.log(tx_hash);

    var tx_hash = await this.electionInstance.methods.calculateVotes().send({
      from:this.model.accounts[0],gas:600000 
    }).on("receipt", receipt => {
      this.setStatus("Votes tallied Successfully!");
      console.log("added");
      console.log(receipt);
    });
    console.log("Tx hash : " , tx_hash);


    this.updateElection(3);
  }

  updateElection(status:number) {
    let data = {
      label:this.elections[0].label,
      status
    } 
    let url = "/v1/election/update_status/";
    console.log("inside update elections ", url);
    this.http.post(url, data).subscribe(
      res => {
        console.log(res);
        this.elections[0] =res;
        this.election = res as any;
        if(status==1){
          this.setStatusShort("Success! Voting phase started");
        }
        else if(status==2){
          this.setStatusShort("Success! Voting phase ended")
        }
        else if(status==3){
          this.setStatusShort("Success! Votes tallied. Voting result declared")
        }
      },
      error => {
        console.log(error);
      }
    );
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
        this.elections =res;
        this.election = res[0];
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
