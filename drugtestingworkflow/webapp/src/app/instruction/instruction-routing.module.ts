import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { InstructionComponent } from './instruction.component'
const routes: Routes = [
  {
    path: '',
    component: InstructionComponent
  }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class InstructionRoutingModule { }
