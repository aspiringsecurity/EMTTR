import { Component, OnInit } from '@angular/core';

import { DataService, Arm} from '../../shared/data/data.service'

@Component({
  selector: 'app-new-study',
  templateUrl: './new-study.component.html',
  styleUrls: ['./new-study.component.scss']
})
export class NewStudyComponent implements OnInit {

  arm : Arm

  constructor(
    private dataService : DataService
  ) { 
    this.arm = new Arm('','','','')

  }

  ngOnInit() {
  }

}
