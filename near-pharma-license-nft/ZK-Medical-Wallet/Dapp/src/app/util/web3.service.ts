import { Injectable } from '@angular/core';
import contract from 'truffle-contract';
import { Subject } from 'rxjs';
declare let require: any;
const Web3 = require('web3');
import { HttpClientModule } from '@angular/common/http';

declare let window: any;


@Injectable()
export class Web3Service {
    private web3: any;
    private accounts: string[];
    public ready = false;
    public uuid: number;
    // public election_status = 0;
    public election = {
          label: "Test Election",
          start_time: "April 21, 2019",
          end_time: "May 21, 2019",
          status: 0 // 0 - yet to start, 1- ongoing, 2 - completed
        };
    public election_label: string;

    public accountsObservable = new Subject<string[]>();

    constructor() {
        window.addEventListener('load', async () => {
            // Modern dapp browsers...
            if (window.ethereum) {
                window.web3 = new Web3(window.ethereum);
                try {
                    // Request account access if needed
                    await window.ethereum.enable();
                    // Acccounts now exposed
                    console.log("inside window ethereum");
                    console.log(window.web3);
                    // this.web3.eth.sendTransaction({/* ... */ });
                } catch (error) {
                    // User denied account access...
                    console.log('Cannot send the transaction')
                }
            }
            // Legacy dapp browsers...
            else if (window.web3) {
                window.web3 = new Web3(this.web3.currentProvider);
                // Acccounts always exposed
                this.web3.eth.sendTransaction({/* ... */ });
            }
            // Non-dapp browsers...
            else {
                console.log('Non-Ethereum browser detected. You should consider trying MetaMask!');
            }
        });
        setInterval(() => this.refreshAccounts(), 100000);
    }

    public setElectionLabel(label: string){
        this.election_label = label;
        console.log("Election label set: ", this.election_label);
    }

    public getElectionLabel(){
        return this.election_label;
    }

    // public bootstrapWeb3() {
    //   // Checking if Web3 has been injected by the browser (Mist/MetaMask)
    //   if (typeof window.web3 !== 'undefined') {
    //     // Use Mist/MetaMask's provider
    //     this.web3 = new Web3(window.web3.currentProvider);
    //     console.log(' web3 MetaMask!');
    //   } else {
    //     console.log('No web3? You should consider trying MetaMask!');

    //     // Hack to provide backwards compatibility for Truffle, which uses web3js 0.20.x
    //     Web3.providers.HttpProvider.prototype.sendAsync = Web3.providers.HttpProvider.prototype.send;
    //     // fallback - use your fallback strategy (local node / hosted node + in-dapp id mgmt / fail)
    //     this.web3 = new Web3(new Web3.providers.HttpProvider('http://127.0.0.1:8545'));
    //   }

    //   setInterval(() => this.refreshAccounts(), 5000);
    // }

    public async artifactsToContract(artifacts) {
        if (!window.web3) {
            const delay = new Promise(resolve => setTimeout(resolve, 100));
            await delay;
            return await this.artifactsToContract(artifacts);
        }

        const contractAbstraction = contract(artifacts);
        contractAbstraction.setProvider(window.web3.currentProvider);
        return contractAbstraction;

    }

    private refreshAccounts() {
        window.web3.eth.getAccounts((err, accs) => {
            console.log('Refreshing accounts');
            if (err != null) {
                console.warn('There was an error fetching your accounts.');
                return;
            }
            console.log('accounts : ');
            console.log(accs);
            // console.log(window.web3.eth.getBalance(accs[0]));
            // Get the initial account balance so it can be displayed.
            if (accs.length === 0) {
                console.warn('Couldn\'t get any accounts! Make sure your Ethereum client is configured correctly.');
                return;
            }

            if (!this.accounts || this.accounts.length !== accs.length || this.accounts[0] !== accs[0]) {
                console.log('Observed new accounts');

                this.accountsObservable.next(accs);
                this.accounts = accs;
            }

            this.ready = true;
        });
    }
    public async sendEth(value, receiver) {
        console.log(" sending eth : " + value + " to " + receiver);
        // await window.web3.eth.getTransactionCount("0x779C680F2dED76249AA2139F9CaC64eA47d68C2D").then(async (nonce) => {
        //     let tx = await window.web3.eth.sendTransaction({ from: "0x779C680F2dED76249AA2139F9CaC64eA47d68C2D", nonce: nonce, to: receiver, value: value })
        //     return tx;
        // })
        var nonce = 2;
        nonce = await window.web3.eth.getTransactionCount("0x779C680F2dED76249AA2139F9CaC64eA47d68C2D");
        console.log("nonce : " + nonce);
        var value_in_wei = window.web3.utils.toWei(value);
        let tx = await window.web3.eth.sendTransaction({ from: "0x779C680F2dED76249AA2139F9CaC64eA47d68C2D", nonce: nonce, to: receiver, value: value_in_wei });
        // let tx = await window.web3.eth.sendTransaction({ from: "0x779C680F2dED76249AA2139F9CaC64eA47d68C2D", to: receiver, value: value });

        return tx;

        // return nonce;
    }
    public getProvider() {
        return window.web3.currentProvider;
    }

    // Get the Nonce of an account
    public async getNonce(account) {
        const nonce = await window.web3.eth.getTransactionCount(account);
        return nonce;
    }

}
