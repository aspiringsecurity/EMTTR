import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { HomeRoutingModule } from './home-routing.module';
import { HomeComponent } from './home.component';

import { NgbModule } from '@ng-bootstrap/ng-bootstrap';
import { Ng2SmartTableModule } from 'ng2-smart-table';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { NewTaskComponent } from './new-task/new-task.component';
import { TaskDetailComponent } from './task-detail/task-detail.component';
import { TaskListComponent } from './task-list/task-list.component';


@NgModule({
  imports: [
    CommonModule,
    HomeRoutingModule,
    Ng2SmartTableModule,
    FormsModule,
    NgbModule
  ],
  declarations: [HomeComponent, NewTaskComponent, TaskDetailComponent, TaskListComponent]
})
export class HomeModule { }
