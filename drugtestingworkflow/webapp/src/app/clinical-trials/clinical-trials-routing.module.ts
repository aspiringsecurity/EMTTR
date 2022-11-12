import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';

import { ClinicalTrialsComponent } from './clinical-trials.component'
import { AuditComponent } from './audit/audit.component';
import { VolunteerComponent } from './volunteer/volunteer.component';
import { NewStudyComponent } from './new-study/new-study.component'

const routes: Routes = [
  {
    path: '',
    component: ClinicalTrialsComponent
  },
  {
    path: 'new',
    component: NewStudyComponent
  },
  {
    path: 'audit',
    component: AuditComponent
  },
  {
    path: 'volunteer',
    component: VolunteerComponent
  }
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class ClinicalTrialsRoutingModule { }
