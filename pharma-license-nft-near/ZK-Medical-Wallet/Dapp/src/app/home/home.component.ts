import { Component, OnInit } from "@angular/core";
import { HttpClient } from "@angular/common/http";
import { MatSnackBar } from "@angular/material";
import { Web3Service } from "../util/web3.service";
import { Router } from '@angular/router';
import {MatInputModule} from '@angular/material/input';


@Component({
  selector: 'app-home',
  templateUrl: './home.component.html',
  styleUrls: ['./home.component.css']
})
export class HomeComponent implements OnInit {

  gen: boolean; 
  uname: string ='';
  uid: string = '';
  passwd: string = '';
  links = [
    {
      label: 'Voter Registration',
      url: "/kyc"
    },
    {
      label: 'Voter Login',
      url: "/dashboard"
    },
    {
      label: 'KYC Verifier Portal',
      url: "/kycverifier"
    },
    {
      label: 'Admin Portal',
      url: "/admin",
    }
  ];

  constructor(
    private matSnackBar: MatSnackBar,
    private http: HttpClient,
    private router: Router
  ) { }

  ngOnInit() {
    this.gen=false;
  }

  register() {
    let obj = {    
      "uname": this.uname, 
      "passwd": this.passwd,
      "uid": this.uid
    }
   window.localStorage.setItem("iden", JSON.stringify(obj));
   this.setStatus("Registered Successfully!");
  //  this.uname ='';
  //  this.uid = '';
  //  this.passwd = '';
   this.router.navigateByUrl('/login2')
  }

  genPass() {
    let url = "http://localhost:8080/passGen";
    this.http.get(url).subscribe(
      res => {
        console.log(res);
        this.passwd = res.hash;
      },
      error => {
        // console.log(error.error.text);

      }
    );
  }

  setStatus(status) {
    this.matSnackBar.open(status, null, { duration: 5000 });
  }
}
