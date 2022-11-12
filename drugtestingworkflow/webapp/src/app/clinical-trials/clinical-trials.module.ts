import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';

import { ClinicalTrialsRoutingModule } from './clinical-trials-routing.module';
import { ClinicalTrialsComponent } from './clinical-trials.component';
import { AuditComponent } from './audit/audit.component';
import { VolunteerComponent } from './volunteer/volunteer.component';
import { Ng2SmartTableModule } from 'ng2-smart-table';
import { NewStudyComponent } from './new-study/new-study.component';

import { NgbModule } from '@ng-bootstrap/ng-bootstrap';

@NgModule({
  imports: [
    CommonModule,
    ClinicalTrialsRoutingModule,
    Ng2SmartTableModule,
    FormsModule,
    NgbModule
  ],
  declarations: [ClinicalTrialsComponent, AuditComponent, VolunteerComponent, NewStudyComponent]
})
export class ClinicalTrialsModule { }
