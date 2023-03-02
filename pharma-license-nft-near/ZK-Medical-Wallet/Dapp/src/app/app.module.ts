import { BrowserModule } from "@angular/platform-browser";
import { NgModule } from "@angular/core";
import { FormsModule } from "@angular/forms";
import { HttpClientModule } from "@angular/common/http";
import { BrowserAnimationsModule } from "@angular/platform-browser/animations";
import { MatMomentDateModule } from "@angular/material-moment-adapter";
import { ChartsModule } from 'ng2-charts';
import {
  MatButtonModule,
  MatNativeDateModule,
  MatFormFieldModule,
  MatCheckboxModule,
  MatInputModule,
  MatSelectModule,
  MatRadioModule,
  MatCardModule,
  MatTabsModule,
  MatToolbarModule,
  MatSidenavModule,
  MatIconModule,
  MatListModule,
  MatTableModule,
  MatPaginatorModule,
  MatSortModule,
  MatGridListModule,
  MatMenuModule,
  MatChipsModule,
  MatSlideToggleModule,
  MatSlideToggle,
} from "@angular/material";
import {MatDatepickerModule} from '@angular/material/datepicker';
import { AppComponent } from "./app.component";
import { MetaModule } from "./meta/meta.module";
import { CommonModule } from "@angular/common";
import { TabNavBarBasicExampleComponent } from "./tab-nav-bar-basic-example/tab-nav-bar-basic-example.component";
import { AppRoutingModule } from "./app-routing.module";
import { LoginComponent } from "./login/login.component";
import { VoteComponent } from "./vote/vote.component";
import { KycComponent } from "./kyc/kyc.component";
import { VerifyComponent } from "./verify/verify.component";
import { AdminComponent } from "./admin/admin.component";
import { HomeComponent } from "./home/home.component";
import { DashboardComponent } from "./dashboard/dashboard.component";
import { UserLoginComponent } from "./user-login/user-login.component";
import { KycVerifierComponent } from './kyc-verifier/kyc-verifier.component';
import { ElectionComponent } from './election/election.component';
import { Login2Component } from './login2/login2.component';
import { RegisterComponent } from './register/register.component';
import { BegVotingComponent } from './beg-voting/beg-voting.component';
import { CryptonComponent } from './crypton/crypton.component';

@NgModule({
  declarations: [
    AppComponent,
    TabNavBarBasicExampleComponent,
    LoginComponent,
    VoteComponent,
    KycComponent,
    VerifyComponent,
    AdminComponent,
    HomeComponent,
    DashboardComponent,
    UserLoginComponent,
    KycVerifierComponent,
    ElectionComponent,
    Login2Component,
    RegisterComponent,
    BegVotingComponent,
    CryptonComponent
  ],
  imports: [
    BrowserAnimationsModule,
    CommonModule,
    MatButtonModule,
    MatCardModule,
    MatDatepickerModule,
    // MatMomentDateModule,
    MatSlideToggleModule,
    MatFormFieldModule,
    MatInputModule,
    MatNativeDateModule,
    MatToolbarModule,
    BrowserModule,
    ChartsModule,
    FormsModule,
    HttpClientModule,
    MetaModule,
    MatTabsModule,
    AppRoutingModule,
    MatCheckboxModule,
    MatSelectModule,
    MatRadioModule,
    MatSidenavModule,
    MatIconModule,
    MatListModule,
    MatTableModule,
    MatPaginatorModule,
    MatSortModule,
    MatGridListModule,
    MatMenuModule,
    MatChipsModule
  ],
  providers: [MatMomentDateModule],
  bootstrap: [AppComponent]
})
export class AppModule {}
