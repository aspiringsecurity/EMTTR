import { Component, OnInit } from '@angular/core';

import { EthService } from "../../shared/eth/eth.services"
import { DataService } from "../../shared/data/data.service"
import { Router, ActivatedRoute } from "@angular/router";

@Component({
  selector: 'app-task-list',
  templateUrl: './task-list.component.html',
  styleUrls: ['./task-list.component.scss']
})
export class TaskListComponent implements OnInit {

  totalTask = 0
  index

  tasks = []

  constructor(
    private dataService: DataService,
    private ethService: EthService,
    private router: Router,
    private route: ActivatedRoute
  ) {

  }

  goTo(id) {
    this.router.navigate(['/home/task/'+id]);
  }

  ngOnInit() {
    //setTimeout(()=>{
    //console.log("tasks-->",this.ethService.getTask())
    // },10000)

    this.ethService.reload.subscribe(
      tick => {
        this.tasks = []

        let index = this.ethService.getTask()
        let ids = Object.keys(index)
        this.totalTask = ids.length
        let count = 1
        for (let i of ids) {
          console.log('i-->', index[i])
          let task = {
            owner: index[i][0],
            title: index[i][1],
            tokenURI: index[i][3],
            status: index[i][5],
            id: count

          }
          this.dataService.getTokenURI(task.tokenURI).subscribe(
            payload => {


              task['deposit'] = payload.deposit
              task['description'] = payload.description
              task['smiles'] = payload.smiles

              console.log("task-->", task)
              this.tasks.push(task)
            }
          )
          count += 1
        }


      }
    )

    this.ethService.reload.next(true)


  }

}
