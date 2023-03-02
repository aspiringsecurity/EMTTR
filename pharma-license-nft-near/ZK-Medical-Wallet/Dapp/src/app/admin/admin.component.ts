import { Component, OnInit } from "@angular/core";
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
  selector: "app-admin",
  templateUrl: "./admin.component.html",
  styleUrls: ["./admin.component.css"]
})
export class AdminComponent implements OnInit {
  ElectionInstance: any;

  displayedColumns: string[] = ["Label", "Start Time", "End Time", "Action"];

  elections : any;

  model = {
    accounts: null,
    primary_account: null
  };

  constructor(
    private matSnackBar: MatSnackBar,
    private http: HttpClient,
    private web3Service: Web3Service,
    private router: Router
  ) {}

  async ngOnInit() {
    console.log("OnInit: " + this.web3Service);
    console.log(this);
    this.watchAccount();
    this.model.accounts = await web3.eth.getAccounts();
    console.log(this.model.accounts);
    // this.elections[0] = this.web3Service.election;
    // console.log("election from web3 : ", this.elections[0]);
    // console.log("new election : ", this.elections);

    this.web3Service
      .artifactsToContract(election_artifact)
      .then(ElectionAbstraction => {
        this.ElectionInstance = ElectionAbstraction;
        this.ElectionInstance.deployed().then(deployed => {
          console.log(deployed);
          this.ElectionInstance = deployed;
        });
      });

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



