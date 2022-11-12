import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { InstructionRoutingModule } from './instruction-routing.module';
import { InstructionComponent } from './instruction.component';

@NgModule({
  imports: [
    CommonModule,
    InstructionRoutingModule
  ],
  declarations: [InstructionComponent]
})
export class InstructionModule { }
