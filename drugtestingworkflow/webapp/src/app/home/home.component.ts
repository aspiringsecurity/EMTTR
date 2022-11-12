import { Component, OnInit } from '@angular/core';
import { EthService } from "../shared/eth/eth.services"

@Component({
  selector: 'app-home',
  templateUrl: './home.component.html',
  styleUrls: ['./home.component.scss']
})
export class HomeComponent implements OnInit {

  status = {
    total: 0,
    pending: 0,
    screening: 0,
    screened: 0
  }

  constructor(
    private ethService: EthService
  ) {

  }

  ngOnInit() {
    setTimeout(() => {
      let index = this.ethService.getTask()
      let ids = Object.keys(index)
      this.status.total = ids.length
      for (let i of ids) {
        console.log('i-->', index[i])
        switch(index[i][5]) {
          case "0":
            this.status.pending +=1
            break;
          case "1":
            this.status.screening +=1
            break;
          case "2":
            this.status.screened +=1
            break;
        }
          
      }
    }, 3000)

  }

}
