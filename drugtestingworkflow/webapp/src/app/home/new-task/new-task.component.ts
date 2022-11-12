import { Component, OnInit } from '@angular/core';

import { DataService, Task } from '../../shared/data/data.service'
import { EthService } from '../../shared/eth/eth.services'

@Component({
  selector: 'app-new-task',
  templateUrl: './new-task.component.html',
  styleUrls: ['./new-task.component.scss']
})
export class NewTaskComponent implements OnInit {
  address: string
  task: Task

  constructor(
    private dataService: DataService,
    private ethService : EthService
  ) {
    this.task = new Task();
    this.ethService.getAccounts().then(
      accounts => {
          if (accounts[0]) {
              this.address = accounts[0]
          }
      }
  )
  }

  ngOnInit() {

  }

  clear() {
    this.task = new Task();
  }


  save() {

    const id = this.dataService.generateGuid()
    this.dataService.createTask(id, this.task).subscribe(
      result => {
        if (result.ok) {
          console.log("result --> ", result)
          console.log("id --> ", result.id)
          console.log("rev-->", result.rev)
          const url = this.dataService.getBaseURL()+result.id
          this.ethService.createTask(this.address, this.task, url,result.rev ).send({from: this.address})
          

        }


      }
    )
  }

}
