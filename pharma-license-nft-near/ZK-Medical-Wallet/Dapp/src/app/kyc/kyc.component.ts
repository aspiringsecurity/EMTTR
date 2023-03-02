import { Component, OnInit } from "@angular/core";
import { EmailValidator } from "@angular/forms";
import { HttpClient } from "@angular/common/http";
import { MatSnackBar } from "@angular/material";
import { Router } from "@angular/router";


@Component({
  selector: "app-kyc",
  templateUrl: "./kyc.component.html",
  styleUrls: ["./kyc.component.css"]
})
export class KycComponent implements OnInit {
  model = {
    kyc_object: null,
    submitted: false,
    kyc_done: false,
    show_status: false,
    uuid_to_verify: null
  };
  dob:Date;
  constituencies: any;

  constructor(private matSnackBar: MatSnackBar, private router: Router, private http: HttpClient) {}

  ngOnInit() {
    this.getConstituencyList();
  }

  add(
    name: string,
    uuid: number,
    mobile: number,
    dob: Date,
    password: string,
    constituency: string
  ): void {

    var data = {
      name,
      uuid,
      mobile,
      dob:this.dob.getTime(),
      dob_string:this.dob.toDateString(),
      password,
      constituency
    };
    console.log(data);
    var url = "/v1/kyc/info/add/";
    this.http.post(url, data).subscribe(res => {
      console.log(res);
      this.model.kyc_object = res;
      this.model.submitted = true;
      this.setStatus(
        "Your data is submitted for verification. Come back after some time to check the status"
      );
      this.router.navigateByUrl("/home");
    });
  }

  verify(uuid: number) {
    var url = "/v1/kyc/info/" + uuid.toString() + "/";
    this.http.get(url).subscribe(
      res => {
        console.log(res);
        this.model.kyc_done = res["kyc_done"];
        this.model.show_status = true;
        if (this.model.kyc_done) {
          this.setStatus("Kyc Verified!");
        } else {
          this.setStatus("Verification Peding. Check after some time");
        }
        // this.setStatus("Your data is submitted for verification. Come back after some time to check the status")
      },
      error => {
        console.log(error);
        this.model.show_status = true;
      }
    );
  }
  setStatus(status) {
    this.matSnackBar.open(status, null, { duration: 3000 });
  }

  set_uuid(e) {
    this.model.uuid_to_verify = e.target.value;
    this.model.show_status = false;
  }

  set_date(e) {
    console.log(e.target.value + typeof(e.target.value));
    let date = new Date(e.target.value);
    // let date = e.target.value.toDate();
    this.dob = date;
    console.log(date.getTime(), date.toDateString());
    
  }

  getConstituencyList() {
    console.log("getting the list of constituencies ...");
    let url = "/v1/constituency/";
    this.http.get(url).subscribe(res => {
      console.log(res);
      this.constituencies = res;
    });
  }

  home() {
    this.router.navigateByUrl('/home');
}
}
