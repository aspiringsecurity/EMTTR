import { Component } from '@angular/core';

import { EthService } from "./shared/eth/eth.services"
import { AuthService } from './shared/auth/auth.service'
import { Router, ActivatedRoute } from "@angular/router";
@Component({
    selector: 'app-root',
    templateUrl: './app.component.html'
})
export class AppComponent {



    private loading = true

    constructor(
        private ethService: EthService,
        private authService  :AuthService,
        private router : Router,
        private route : ActivatedRoute
    ) {
        this.ethService.getAccounts().then(
            accounts => {
                this.loading = false
                console.log(accounts)
                if (accounts[0]) {
                    this.authService.logged = true
                } else {
                    this.router.navigate(['/error']);
                }
             }
        ).catch(
            err=>{
                this.loading = false
                console.log('metamask error!')
                this.router.navigate(['/error']);
            }
        )
    }
}