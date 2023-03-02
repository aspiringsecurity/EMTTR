import { Component, OnInit } from '@angular/core';
import { HttpClient } from "@angular/common/http";
import { Router } from '@angular/router';
import { MatSnackBar } from "@angular/material";

const zkURL = "http://18.21.191.249:3000/";


interface Candidate {
  val: string;
  name: string;
}

@Component({
  selector: 'app-beg-voting',
  templateUrl: './beg-voting.component.html',
  styleUrls: ['./beg-voting.component.css']
})
export class BegVotingComponent implements OnInit {

  success: any;

  candidates: Candidate[] = [
    {val: 'bjp-0', name: 'BJP'},
    {val: 'congress-1', name: 'Congress'},
    {val: 'bsp-2', name: 'BSP'}
  ];
  verified=false;
  // voting_passwd: string='';
  voted_cdt: string ='';
  constructor(
    private matSnackBar: MatSnackBar,
    private http: HttpClient,
    private router: Router
  ) { }

  ngOnInit() {
    this.success = false;
    this.verified = true;
  }

  vote() {
    console.log(this.voted_cdt);
    console.log(this.voting_passwd)
    let url3 = zkURL + "vote/proof?secret=" + this.voting_passwd + "&x1=234&x2=789&x3=" + this.voting_passwd;
    this.http.get(url3).subscribe(
      res => { console.log(res); this.success = true },
      error => { console.log(error) }
    )
  }

  vA() {
    let url = "http://localhost:8080/age/proof";
    this.http.get(url).subscribe(
      res => {
        console.log(res);
        let url2 = zkURL + "age/verify?ageproof=" + JSON.stringify(res);
        this.http.get(url2).subscribe(
          res2 => {
            console.log(res2)
            this.verified = !res2;
            this.setStatus("Age Verified Successfully!")
          },
          error => {

          }
        )
      },
      error => {
        console.log(error.error.text);

      }
    );
  }
  setStatus(status) {
    this.matSnackBar.open(status, null, { duration: 3000 });
  }
}
