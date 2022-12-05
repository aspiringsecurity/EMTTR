import { Component, OnInit, Input } from '@angular/core';
import { MatSnackBar } from "@angular/material";

@Component({
  selector: 'app-register',
  templateUrl: './register.component.html',
  styleUrls: ['./register.component.css']
})
export class RegisterComponent implements OnInit {
  @Input() uname: string;
  showSpinner: boolean;
  voting_passwd: string = '';
  constructor(
    private matSnackBar: MatSnackBar
  ) { }

  ngOnInit() {
    this.showSpinner=false;
  }

  async enterPasswd() {
    this.showSpinner = true;
    let obj = {    
      "passwd": this.voting_passwd,
       "x": 256,
       "a": 512
    }
   window.localStorage.setItem("commitmentobj", JSON.stringify(obj));
    setTimeout(()=>{    
      this.showSpinner = false;
      this.setStatus('Voting Password Set Successfully');
      console.log(obj)
    }, 1000);
  }
 
  setStatus(status) {
    this.matSnackBar.open(status, null, { duration: 5000 });
  }
}