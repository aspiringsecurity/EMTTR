import { NgModule, Component } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { HomeComponent } from './home.component';
import { NewTaskComponent } from './new-task/new-task.component';
import { TaskDetailComponent } from './task-detail/task-detail.component'
import { TaskListComponent } from './task-list/task-list.component'

const routes: Routes = [
  {
    path : '',
    component : HomeComponent
  },
  {
    path :'task/new',
    component : NewTaskComponent
  },
  {
    path : 'task',
    component : TaskListComponent
  },
  {
    path :'task/:id',
    component : TaskDetailComponent
  }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class HomeRoutingModule { }
