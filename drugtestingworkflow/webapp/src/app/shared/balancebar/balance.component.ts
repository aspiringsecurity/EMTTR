import { Component } from '@angular/core';
import { Router, ActivatedRoute } from "@angular/router";
import { EthService } from '../eth/eth.services'

@Component({
    selector: 'app-balance',
    templateUrl: './balance.component.html',
    styleUrls: ['./balance.component.scss']
})

export class BalanceComponent {
    balance = 0
    balanceETH = 0
    address: string
    constructor(
        private ethService: EthService,
        private router: Router,
        private route: ActivatedRoute
    ) {
        this.ethService.getAccounts().then(
            accounts => {
                if (accounts[0]) {
                    console.log('account --> ', accounts[0])
                    this.address = accounts[0]
                    this.updateBalance()
                }
            }
        )
    }
    
    reload() {
        console.log('reloading')
        this.ethService.reload.next(true)
        this.balance = 0
        this.ethService.getAccounts().then(
            accounts => {
                if (accounts[0]) {
                    this.address = accounts[0]
                    this.updateBalance()
                }
            }
        )

    }
    gotoNewStudy() {
        this.router.navigate(['/trials/new']);
    }

    gotoNewTask() {
        this.router.navigate(['/home/task/new']);
    }
    facet() {
        if (!this.address) {
            return
        }
        this.ethService.facet(this.address).send({from: this.address})
    }

    updateBalance() {
        if (!this.address) {
            return
        }
        console.log('get balance')
        this.ethService.getBalance(this.address).then(
            result => {
                console.log('result-->', result)
                //this.balance = this.ethService.convertEth(result)
                this.balanceETH= this.ethService.convertEth(result)
            }
        ).catch(
            err => {
                console.error(err)
            }
        )
        this.ethService.getERC20Balance(this.address).then(
            result=>{
                console.log('ERC20-->',result)
                this.balance = this.ethService.convertEth(result.toString())
            }
        ).catch(
            err=>{
                console.error(err)
            }
        )

         
    }
}
