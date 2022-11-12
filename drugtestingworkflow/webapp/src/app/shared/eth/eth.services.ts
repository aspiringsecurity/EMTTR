import { Router } from '@angular/router';
import { Injectable } from '@angular/core';
import * as Web3 from 'web3';
import { environment } from "../../../environments/environment"



declare let require: any;
declare let window: any;

import { ethers } from 'ethers';
import { Task } from '../data/data.service';
import { Subject } from 'rxjs';

@Injectable()
export class EthService {


    private web3: any;

    constract = "0x9a30b7f98b84cbaba8b9787a8394a6332ef3b5fa"

    abi = [
        {
            "constant": true,
            "inputs": [
                {
                    "name": "_task_id",
                    "type": "uint256"
                }
            ],
            "name": "findResult",
            "outputs": [
                {
                    "name": "",
                    "type": "uint256[]"
                }
            ],
            "payable": false,
            "stateMutability": "view",
            "type": "function"
        },
        {
            "constant": false,
            "inputs": [
                {
                    "name": "_task_id",
                    "type": "uint256"
                }
            ],
            "name": "award",
            "outputs": [],
            "payable": false,
            "stateMutability": "nonpayable",
            "type": "function"
        },
        {
            "constant": true,
            "inputs": [],
            "name": "name",
            "outputs": [
                {
                    "name": "",
                    "type": "string"
                }
            ],
            "payable": false,
            "stateMutability": "view",
            "type": "function"
        },
        {
            "constant": false,
            "inputs": [
                {
                    "name": "spender",
                    "type": "address"
                },
                {
                    "name": "value",
                    "type": "uint256"
                }
            ],
            "name": "approve",
            "outputs": [
                {
                    "name": "",
                    "type": "bool"
                }
            ],
            "payable": false,
            "stateMutability": "nonpayable",
            "type": "function"
        },
        {
            "constant": true,
            "inputs": [],
            "name": "numTask",
            "outputs": [
                {
                    "name": "",
                    "type": "uint256"
                }
            ],
            "payable": false,
            "stateMutability": "view",
            "type": "function"
        },
        {
            "constant": false,
            "inputs": [
                {
                    "name": "_id",
                    "type": "uint256"
                }
            ],
            "name": "approveTask",
            "outputs": [],
            "payable": false,
            "stateMutability": "nonpayable",
            "type": "function"
        },
        {
            "constant": true,
            "inputs": [],
            "name": "totalSupply",
            "outputs": [
                {
                    "name": "",
                    "type": "uint256"
                }
            ],
            "payable": false,
            "stateMutability": "view",
            "type": "function"
        },
        {
            "constant": true,
            "inputs": [
                {
                    "name": "_id",
                    "type": "uint256"
                }
            ],
            "name": "getTask",
            "outputs": [
                {
                    "name": "",
                    "type": "address"
                },
                {
                    "name": "",
                    "type": "string"
                },
                {
                    "name": "",
                    "type": "uint256"
                },
                {
                    "name": "",
                    "type": "string"
                },
                {
                    "name": "",
                    "type": "uint256"
                },
                {
                    "name": "",
                    "type": "int256"
                }
            ],
            "payable": false,
            "stateMutability": "view",
            "type": "function"
        },
        {
            "constant": false,
            "inputs": [
                {
                    "name": "owner",
                    "type": "address"
                },
                {
                    "name": "task_id",
                    "type": "uint256"
                },
                {
                    "name": "result",
                    "type": "int256"
                },
                {
                    "name": "metadataURL",
                    "type": "string"
                },
                {
                    "name": "metadataHash",
                    "type": "string"
                }
            ],
            "name": "submitResult",
            "outputs": [],
            "payable": false,
            "stateMutability": "nonpayable",
            "type": "function"
        },
        {
            "constant": false,
            "inputs": [
                {
                    "name": "owner",
                    "type": "address"
                },
                {
                    "name": "title",
                    "type": "string"
                },
                {
                    "name": "deposit",
                    "type": "uint256"
                },
                {
                    "name": "metadataURL",
                    "type": "string"
                },
                {
                    "name": "metadataHash",
                    "type": "string"
                }
            ],
            "name": "createTask",
            "outputs": [],
            "payable": false,
            "stateMutability": "nonpayable",
            "type": "function"
        },
        {
            "constant": true,
            "inputs": [
                {
                    "name": "",
                    "type": "address"
                }
            ],
            "name": "balances",
            "outputs": [
                {
                    "name": "",
                    "type": "uint256"
                }
            ],
            "payable": false,
            "stateMutability": "view",
            "type": "function"
        },
        {
            "constant": true,
            "inputs": [],
            "name": "decimals",
            "outputs": [
                {
                    "name": "",
                    "type": "uint8"
                }
            ],
            "payable": false,
            "stateMutability": "view",
            "type": "function"
        },
        {
            "constant": false,
            "inputs": [
                {
                    "name": "_name",
                    "type": "string"
                },
                {
                    "name": "_symbol",
                    "type": "string"
                },
                {
                    "name": "_decimals",
                    "type": "uint8"
                },
                {
                    "name": "_amount",
                    "type": "uint256"
                }
            ],
            "name": "initiateToken",
            "outputs": [],
            "payable": false,
            "stateMutability": "nonpayable",
            "type": "function"
        },
        {
            "constant": true,
            "inputs": [],
            "name": "getSender",
            "outputs": [
                {
                    "name": "",
                    "type": "address"
                }
            ],
            "payable": false,
            "stateMutability": "view",
            "type": "function"
        },
        {
            "constant": true,
            "inputs": [],
            "name": "isTokenInitialized",
            "outputs": [
                {
                    "name": "",
                    "type": "bool"
                }
            ],
            "payable": false,
            "stateMutability": "view",
            "type": "function"
        },
        {
            "constant": true,
            "inputs": [
                {
                    "name": "owner",
                    "type": "address"
                }
            ],
            "name": "balanceOf",
            "outputs": [
                {
                    "name": "",
                    "type": "uint256"
                }
            ],
            "payable": false,
            "stateMutability": "view",
            "type": "function"
        },
        {
            "constant": false,
            "inputs": [
                {
                    "name": "to",
                    "type": "address"
                }
            ],
            "name": "mintTo",
            "outputs": [],
            "payable": false,
            "stateMutability": "nonpayable",
            "type": "function"
        },
        {
            "constant": false,
            "inputs": [
                {
                    "name": "_id",
                    "type": "uint256"
                },
                {
                    "name": "_status",
                    "type": "int256"
                }
            ],
            "name": "setTaskStatus",
            "outputs": [],
            "payable": false,
            "stateMutability": "nonpayable",
            "type": "function"
        },
        {
            "constant": false,
            "inputs": [
                {
                    "name": "_id",
                    "type": "uint256"
                }
            ],
            "name": "cancelTask",
            "outputs": [],
            "payable": false,
            "stateMutability": "nonpayable",
            "type": "function"
        },
        {
            "constant": true,
            "inputs": [],
            "name": "symbol",
            "outputs": [
                {
                    "name": "",
                    "type": "string"
                }
            ],
            "payable": false,
            "stateMutability": "view",
            "type": "function"
        },
        {
            "constant": true,
            "inputs": [
                {
                    "name": "_id",
                    "type": "uint256"
                }
            ],
            "name": "getResult",
            "outputs": [
                {
                    "name": "",
                    "type": "address"
                },
                {
                    "name": "",
                    "type": "uint256"
                },
                {
                    "name": "",
                    "type": "int256"
                },
                {
                    "name": "",
                    "type": "int256"
                },
                {
                    "name": "",
                    "type": "string"
                }
            ],
            "payable": false,
            "stateMutability": "view",
            "type": "function"
        },
        {
            "constant": false,
            "inputs": [
                {
                    "name": "_id",
                    "type": "uint256"
                }
            ],
            "name": "approveResult",
            "outputs": [],
            "payable": false,
            "stateMutability": "nonpayable",
            "type": "function"
        },
        {
            "constant": false,
            "inputs": [
                {
                    "name": "to",
                    "type": "address"
                },
                {
                    "name": "value",
                    "type": "uint256"
                }
            ],
            "name": "transfer",
            "outputs": [
                {
                    "name": "",
                    "type": "bool"
                }
            ],
            "payable": false,
            "stateMutability": "nonpayable",
            "type": "function"
        },
        {
            "constant": true,
            "inputs": [
                {
                    "name": "owner",
                    "type": "address"
                },
                {
                    "name": "spender",
                    "type": "address"
                }
            ],
            "name": "allowance",
            "outputs": [
                {
                    "name": "",
                    "type": "uint256"
                }
            ],
            "payable": false,
            "stateMutability": "view",
            "type": "function"
        },
        {
            "constant": false,
            "inputs": [
                {
                    "name": "_id",
                    "type": "uint256"
                },
                {
                    "name": "_status",
                    "type": "int256"
                }
            ],
            "name": "setResultStatus",
            "outputs": [],
            "payable": false,
            "stateMutability": "nonpayable",
            "type": "function"
        },
        {
            "constant": true,
            "inputs": [],
            "name": "numResult",
            "outputs": [
                {
                    "name": "",
                    "type": "uint256"
                }
            ],
            "payable": false,
            "stateMutability": "view",
            "type": "function"
        }
    ]
    loadTask = new Subject()
    reload = new Subject()
    runnable = new Subject()
    tasks = {}

    constructor() {
        this.web3 = new Web3(Web3.givenProvider || "ws://localhost:8546");

        if (typeof this.web3 === 'undefined') {
            console.log('No web3 found, get MetaMask!')
        } else {
            console.log('Web3 found! Start indexing!')

            this.loadTask.subscribe(
                (task : any) =>{
                    const taskId = task.id
                    const contract  = task.contract
                    contract.methods.getTask(taskId).call().then(
                        taskItem =>{
                            //console.log('taskItem-->',taskItem)
                            
                            this.tasks[taskId] = taskItem
                            
                        }
                    )
                }
            )

            this.runnable.subscribe(
                tick => {
                    setTimeout(() => {
                        console.log('tick...')

                        let contractAddress = this.constract;

                        let contract = new this.web3.eth.Contract(this.abi, contractAddress);

                         contract.methods.numTask().call().then(
                            result => {
                            
                                for (let i = 1; i <= result; i++) { 

                                    let task = {
                                        id : i ,
                                        contract : contract
                                    }
                                    this.loadTask.next(task)
                                }
                                this.runnable.next(true)
                            }
                        )

                    }, 3000)
                }
            )

            this.runnable.next(true)

        }




    }

    getTask() {
        return this.tasks
    }

    task(index) {
        let provider = ethers.getDefaultProvider('rinkeby');
        let contractAddress = this.constract;
        let contract = new ethers.Contract(contractAddress, this.abi, provider);
        return contract.getTask(index)
    }
    result(result_id) {
        let provider = ethers.getDefaultProvider('rinkeby');
        let contractAddress = this.constract;
        let contract = new ethers.Contract(contractAddress, this.abi, provider);
        return contract.getResult(result_id)
    }

    

    getAccounts() {
        return this.web3.eth.getAccounts()
    }

    convertEth(wei) {

        return this.web3.utils.fromWei(wei, 'ether')
    }



    getBalance(address) {
        let provider = new ethers.providers.Web3Provider(this.web3.currentProvider);
        return this.web3.eth.getBalance(address)
    }

    facet(address) {
        //let provider = ethers.getDefaultProvider('rinkeby');
        let contractAddress = this.constract;
        //let contract = new ethers.Contract(contractAddress, this.abi, provider);
        let contract = new this.web3.eth.Contract(this.abi, contractAddress);
        console.log(contract)


        return contract.methods.mintTo(address)
    }
    submitResult(address, index, result) {
        const url ="dummy"
        const hash = "dummy"
        let contractAddress = this.constract;
        let contract = new this.web3.eth.Contract(this.abi, contractAddress);
        return contract.methods.submitResult(address, Number(index), Number(result),url, hash)
    }

    approveTask(task_id) {
        let contractAddress = this.constract;
        let contract = new this.web3.eth.Contract(this.abi, contractAddress);
        return contract.methods.approveTask(task_id)
    }   
    approveResult(result_id) {
        let contractAddress = this.constract;
        let contract = new this.web3.eth.Contract(this.abi, contractAddress);
        return contract.methods.approveResult(result_id)
    }
    award(task_id) {
        let contractAddress = this.constract;
        let contract = new this.web3.eth.Contract(this.abi, contractAddress);
        return contract.methods.award(task_id)
    }

    createTask(address, task: Task, url, hash) {
        let contractAddress = this.constract;
        let contract = new this.web3.eth.Contract(this.abi, contractAddress);
        const token = 1000000000000000000 * task.deposit

        return contract.methods.createTask(address, task.title, token + "", url, hash)
    }
    

    getName() {
        // Connect to the network
        let provider = ethers.getDefaultProvider('rinkeby');
        //let privateKey = '0x51AB8CA456ABC9F29E8E0F043987069EE8BAA55C5C7BF93C4429AB3842C8ECE9';
        //let wallet = new ethers.Wallet(privateKey, provider);
        // The address from the above deployment example
        let contractAddress = this.constract;

        // We connect to the Contract using a Provider, so we will only
        // have read-only access to the Contract
        let contract = new ethers.Contract(contractAddress, this.abi, provider);

        return contract.getName()
    }
   

    getERC20Balance(address) {
        let provider = ethers.getDefaultProvider('rinkeby');
        let contractAddress = this.constract;
        let contract = new ethers.Contract(contractAddress, this.abi, provider);
        return contract.balanceOf(address)
    }

    findResult(task_id) {
        let provider = ethers.getDefaultProvider('rinkeby');
        let contractAddress = this.constract;
        let contract = new ethers.Contract(contractAddress, this.abi, provider);
        return contract.findResult(task_id)
    }
    
}