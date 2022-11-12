import { Component, OnInit } from '@angular/core';
import {Router, ActivatedRoute} from "@angular/router";

import { EthService } from "../../shared/eth/eth.services"
import { DataService } from "../../shared/data/data.service"

@Component({
  selector: 'app-task-detail',
  templateUrl: './task-detail.component.html',
  styleUrls: ['./task-detail.component.scss']
})
export class TaskDetailComponent implements OnInit {


  loading : boolean


  address: string

  drug : any
  results = []

  constructor(
    private router: Router,
    private activatedRoute: ActivatedRoute,
    private dataService: DataService,
    private ethService: EthService
  ) { 
    this.ethService.getAccounts().then(
      accounts => {
          if (accounts[0]) {
              this.address = accounts[0]
              console.log('my address-->',this.address)

          }
      }
  )
  }
  

  ngOnInit() {
    this.loading = true
    const id = this.activatedRoute.snapshot.params["id"];
    
    this.ethService.task(id).then(
      result=>{
        console.log('result-->',result)
        let task = {
          owner: result[0],
            title: result[1],
            tokenURI: result[3],
            status: result[5].toString(),
        }
        this.dataService.getTokenURI(task.tokenURI).subscribe(
          payload => {

            task['id'] = id
            task['deposit'] = payload.deposit
            task['description'] = payload.description
            task['smiles'] = payload.smiles
            this.loading = false
            console.log("task-->", task)
            this.drug = task
            this.getResults()
          })

      }
    )
  }

  sendResult(result) {
    console.log('result-->',result.value)
    if (this.address == this.drug.owner) {
      alert("not allow drug owner to send the result")
      return
    }
    //submitResult(aiPlayer1, nextTaskCount, 10, url , h);
    this.ethService.submitResult(this.address,this.drug.id, result.value).send({from: this.address})
  }

  winnerId = 0
  winnerScore = 0

  getResults() {
    this.ethService.findResult(this.drug.id).then(
      result=>{
        this.winnerId = 0
        this.winnerScore = 0
        for (let item of result) {
          
          if (item.toString()!="0") {
            
            const id = (Number) (item.toString())
            this.ethService.result(id).then(
              result=>{
                
                const item  ={
                  id : id,
                  address : result[0],
                  status : result[2].toString(),
                  efficacy : result[3].toString()
                }

                if ((Number) (result[3].toString()) > this.winnerScore) {
                  this.winnerScore = (Number) (result[3].toString())
                  this.winnerId = id
                }

                console.log("item  -->",item)
                this.results.push(item)
              }
            )
          }
        }
      }
    )
  }

  approveTask(task_id) {
    this.ethService.approveTask(Number(task_id)).send({from: this.address})
  }
  approveResult(result_id) {
    this.ethService.approveResult(Number(result_id)).send({from: this.address})
  }
  award(task_id) {
    this.ethService.award(Number(task_id)).send({from: this.address})
  }


}
