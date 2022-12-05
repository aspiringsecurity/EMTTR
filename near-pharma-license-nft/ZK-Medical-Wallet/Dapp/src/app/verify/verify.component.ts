import { Component, OnInit } from "@angular/core";
import { Web3Service } from "../util/web3.service";
import web3 from "web3";
// import { ChartOptions, ChartType, ChartDataSets } from 'chart.js';
// import { Color, BaseChartDirective, Label } from 'ng2-charts';
import elec_artifacts from "../../../build/contracts/Election.json";
import { Router } from "@angular/router";
import {  
  animate,
  state,
  style,
  transition,
  trigger
} from "@angular/animations";
// import { type } from 'os';

const PARTIES = ["BJP", "Congress", "BSP"];

let CONSTITUENCIES;
const RESULT_DATA = [];
// const RESULT_DATA = [
//   {
//     "Sr. No.": 1,
//     "Constituency Name": "Bangalore",
//     "Winning Party": "BJP",
//     "Number of Votes": {
//       BJP: 2,
//       Congress: 1,
//       BSP: 0
//     },
//     vote_hashes: {
//       BJP: [
//         "0xc3eb586d884134a11785f3cae787c62c2202449eac3faf7ad1f7cf019f633d88",
//         "0x032900a7e5a67df376e806808ea9a92da5d78105982ab1e61222977c89a9f78e"
//       ],
//       Congress: [
//         "0xc528c2cdb538fe56e760958e8becfea9b49f524d527252b422cded94d749c88b"
//       ],
//       BSP: []
//     }
//   }
// ];

@Component({
  selector: "app-verify",
  templateUrl: "./verify.component.html",
  styleUrls: ["./verify.component.css"],
  animations: [
    trigger("detailExpand", [
      state("collapsed", style({ height: "0px", minHeight: "0" })),
      state("expanded", style({ height: "*" })),
      transition(
        "expanded <=> collapsed",
        animate("225ms cubic-bezier(0.4, 0.0, 0.2, 1)")
      )
    ])
  ]
})
export class VerifyComponent {
  columnsToDisplay: string[] = ["Sr. No.", "Constituency Name", "Winning Party"];
  dataSource = RESULT_DATA;
  result_party = [{name:"BJP",
                       votes:10},
                       {name:"Congress",
                       votes:7},
                       {name:"BSP",
                       votes:5}];
  hashes: string[];
  ElectionInstance: any;
  parties = PARTIES;
  result_data = [];

  // election result no of votes party wise
  displayedColumns: string[] = ["name", "votes"];
  election_label = 'test';

  public doughnutChartLabels = PARTIES;
  public doughnutChartData = [10, 7, 5];
  public doughnutChartType = 'doughnut';

  model = {
    uuid: "",
    voterCount: 1,
    vote_hash: ""
  };
  constructor(private web3Service: Web3Service, private router: Router) {}



  ngOnInit() {
    this.web3Service
      .artifactsToContract(elec_artifacts)
      .then(ElectionAbstraction => {
        this.ElectionInstance = ElectionAbstraction;
        this.ElectionInstance.deployed().then(async (deployed) => {
          console.log(deployed);
          this.ElectionInstance = deployed;

          CONSTITUENCIES = await this.ElectionInstance.getConstituencies();
          console.log(CONSTITUENCIES);
          this.model.voterCount = await this.ElectionInstance.voterCount();

          this.getResult();
        });
      });

      this.election_label = this.web3Service.getElectionLabel();
      console.log("Retrieved election label:", this.election_label);

    // this.ElectionInstance.

  }

  getVoteHash(adhaar: number, password: string) {
    // password = password.trim();
    //  this.hashes
    let votestring =  adhaar.toString() + password;
    console.log("akash",":","kumar");
    console.log("string to be hashed :",votestring,":type:", typeof(votestring));
    this.model.vote_hash = web3.utils.soliditySha3(votestring);

    console.log(this.model.vote_hash);
  }


  async getResult() {
    console.log("party : ", this.result_party);
    console.log("doughnut : ", this.doughnutChartData);
    console.log(this.model.vote_hash);
    let i = 0;
    let result_object = {
      No: 0,
      "Constituency Name": "",
      "Winning Party": "",
      "Number of Votes": {},
      "vote_hashes":{}
    };
    for (let j = 0; j < CONSTITUENCIES.length; j++) {
      let constituency = CONSTITUENCIES[i];
      console.log(constituency);
      result_object["Sr. No."] = i+1;
      result_object["Constituency Name"] = constituency;
      let winning_party = await this.ElectionInstance.getWinner(constituency);
      result_object["Winning Party"] = winning_party;
      console.log("result object", result_object);


      for (let k = 0; k < PARTIES.length; k++) {
        let party = PARTIES[k];
        console.log(party);
        let vote_count = await this.ElectionInstance.getVoteCount(
          constituency,
          party
        );

        this.result_party[k] = {name:party,votes:vote_count.toNumber()};
        this.doughnutChartLabels[k] = party;
        this.doughnutChartData[k] = vote_count.toNumber();
        


        console.log("votes", vote_count.toString());
        result_object["Number of Votes"][party] = vote_count.toString();
        let vote_hashes = await this.ElectionInstance.getVoteHashes(
          constituency,
          party
        );
        console.log("vote_hashes", vote_hashes);
        result_object["vote_hashes"][party] = vote_hashes;
      }

      this.result_data[i] = result_object;
      this.dataSource = this.result_data;

      console.log("party : ", this.result_party);
      console.log("doughnut : ", this.doughnutChartData);
    }
    console.log("final result object : ", result_object);
    
  }
  
  home() {
    this.router.navigateByUrl('/home');
  }

  // events
  public chartClicked({ event, active }: { event: MouseEvent, active: {}[] }): void {
    console.log(event, active);
  }

  public chartHovered({ event, active }: { event: MouseEvent, active: {}[] }): void {
    console.log(event, active);
  }
}
