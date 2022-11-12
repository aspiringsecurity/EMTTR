import { Routes, RouterModule } from '@angular/router';

//Route for content layout with sidebar, navbar and footer
export const Full_ROUTES: Routes = [
  {
    path: 'changelog',
    loadChildren: './changelog/changelog.module#ChangeLogModule'
  },
  {
    path: 'test',
    loadChildren: './pages/full-layout-page/full-pages.module#FullPagesModule'
  },
  {
    path : 'home',
    loadChildren: './home/home.module#HomeModule'
  },
  {
    path : 'instruction',
    loadChildren: './instruction/instruction.module#InstructionModule'
  },
  {
    path : 'trials',
    loadChildren: './clinical-trials/clinical-trials.module#ClinicalTrialsModule'
  }
];