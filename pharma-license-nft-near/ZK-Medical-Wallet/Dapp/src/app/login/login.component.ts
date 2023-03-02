import { Component, OnInit } from '@angular/core';
import { HttpClient } from '@angular/common/http';
import { MatSnackBar } from '@angular/material';
import { Router } from '@angular/router';
import { Web3Service } from '../util/web3.service';


@Component({
    selector: 'app-login',
    templateUrl: './login.component.html',
    styleUrls: ['./login.component.css']
})
export class LoginComponent implements OnInit {

    model = {
        kyc_object: null,
        submitted: false,
        kyc_done: false,
        show_status: false,
        uuid_to_verify: null,
        uuid: null
    }

    constructor(private matSnackBar: MatSnackBar, private http: HttpClient,
                private router: Router, private web3Service: Web3Service) { }

    ngOnInit() {
    }

    send_otp(uuid: number) {
        this.web3Service.uuid = uuid;
        this.setStatusShort("Sending OTP...");
        this.model.uuid = uuid;
        var url = "/v1/otp/send/uuid/" + uuid.toString() + "/";
        this.http.get(url).subscribe((res) => {
            console.log(res);
            var send_status = res["Status"];
            if (send_status == "Success") {
                this.setStatusShort("OTP Sent")
            }
            else {
                this.setStatusShort("Cannot send OTP. please try again later")
            }
            // this.model.show_status = true;
            // this.setStatus("Your data is submitted for verification. Come back after some time to check the status")
        }, (error) => {
            console.log(error);
            this.setStatusShort("Cannot send OTP. please try again later")
        })
    }

    verify_otp(otp_input: number) {

        this.setStatusShort("Verifying OTP...")
        var url = "/v1/otp/verify/uuid/" + this.model.uuid.toString() + "/" + otp_input.toString() + "/";
        this.http.get(url).subscribe((res) => {
            console.log(res);
            var verify_status = res["Status"];
            var is_matched = res["Details"];
            if (verify_status == "Success") {
                this.setStatusShort("OTP Verified")
            }
            else if (verify_status == "Error") {
                this.setStatusShort("Error : OTP Mismatch.")
            }
            else {
                this.setStatusShort("Cannot Verify OTP. please try again after some time ")
            }
            // this.model.show_status = true;
            // this.setStatus("Your data is submitted for verification. Come back after some time to check the status")
        }, (error) => {
            console.log(error);
            this.setStatusShort("Cannot Verify OTP. please try again after some time ")
        });
    }

    home() {
        this.router.navigateByUrl('/home');
    }

    setStatus(status) {
        this.matSnackBar.open(status, null, { duration: 3000 });
        if(status == "OTP Verified") {
            this.router.navigateByUrl('/vote');
        }
    }
    setStatusShort(status) {
        this.matSnackBar.open(status, null, { duration: 2000 });
        if(status == "OTP Verified") {
            this.router.navigateByUrl('/vote');
        }
    }

}
