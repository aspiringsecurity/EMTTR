import { Component, OnInit, Inject } from '@angular/core';
import {MatDialog, MatDialogRef, MAT_DIALOG_DATA} from '@angular/material/dialog';
import { HttpClient } from "@angular/common/http";
import { Router } from '@angular/router';
import { MatSnackBar } from "@angular/material";

const zkURL = "http://18.21.191.249:3000/";


@Component({
  selector: 'app-login2',
  templateUrl: './login2.component.html',
  styleUrls: ['./login2.component.css']
})

export class Login2Component implements OnInit {

  buttontext: string;
  uname: string = '';
  showwait: boolean;
  register: boolean;

  constructor(
    private matSnackBar: MatSnackBar,
    private http: HttpClient,
    private router: Router

    ) { }

  ngOnInit() {  
    this.showwait = false;
    this.register = false;
  }

  enterUname() {
    this.showwait = true;
    let idenObj = JSON.parse(localStorage.getItem("iden"));
    let passHash = idenObj.passwd;
    if(this.uname == idenObj.uname){
      
        let url = "http://localhost:8080/passAuth";
        this.http.get(url).subscribe(
          res => {
            console.log(res);
            let url2 = zkURL + "password/verify?passproof=" + JSON.stringify(res);
            this.http.get(url2).subscribe(
              res2 => {
                if(res2 == true){
                  this.showwait = false;
                  this.setStatus("Logged In Successfully!");
                  this.router.navigateByUrl('/beg_voting')

                }
              },
              error => {
                console.log(error);
              } 
              
            )
          },
          error => {
            console.log(error.error.text);
    
          }
        );      
    }  
  }
  
  setStatus(status) {
    this.matSnackBar.open(status, null, { duration: 5000 });
  }
}