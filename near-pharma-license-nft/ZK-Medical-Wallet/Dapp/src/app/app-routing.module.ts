import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';
import { LoginComponent } from './login/login.component';
import { UserLoginComponent } from './user-login/user-login.component';
import { VoteComponent } from './vote/vote.component';
import { VerifyComponent } from './verify/verify.component';
import { KycComponent } from './kyc/kyc.component';
import { AdminComponent } from './admin/admin.component';
import { HomeComponent } from './home/home.component';
import { DashboardComponent } from './dashboard/dashboard.component';
import { ElectionComponent } from './election/election.component';
import { KycVerifierComponent } from './kyc-verifier/kyc-verifier.component';
import { Login2Component } from './login2/login2.component';
import { RegisterComponent } from './register/register.component';
import { BegVotingComponent } from './beg-voting/beg-voting.component';
import { CryptonComponent } from './crypton/crypton.component';

const routes: Routes = [
  { path: 'kyc', component: KycComponent },
  { path: 'kycverifier', component: KycVerifierComponent },
  { path: 'otp_verification', component: LoginComponent },
  { path: 'user-login', component: UserLoginComponent },
  { path: 'dashboard', component: DashboardComponent },
  { path: 'vote', component: VoteComponent },
  { path: 'resultsverify', component: VerifyComponent },
  { path: 'admin', component: AdminComponent },
  { path: 'home', component: HomeComponent },
  { path: 'election/:id', component: ElectionComponent },
  { path: '', redirectTo: '/home', pathMatch: 'full' },
  { path: 'login2', component: Login2Component },
  { path: 'register', component: RegisterComponent },
  { path: 'beg_voting', component: BegVotingComponent },


  // try
  { path: 'crypton', component: CryptonComponent }
];
 
@NgModule({
  imports: [ RouterModule.forRoot(routes) ],
  exports: [ RouterModule ]
})

export class AppRoutingModule { }
