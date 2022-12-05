import { Component, OnInit } from '@angular/core';

@Component({
  selector: 'app-tab-nav-bar-basic-example',
  templateUrl: './tab-nav-bar-basic-example.component.html',
  styleUrls: ['./tab-nav-bar-basic-example.component.css']
})
export class TabNavBarBasicExampleComponent {
  links = [
    {
      label: 'KYC',
      url: "/kyc"
    },
    {
      label: 'Login',
      url: "/login"
    },
    {
      label: 'Vote',
      url: "/vote"
    }, {
      label: 'Results/Verify',
      url: "/resultsverify",
     },
     {
      label: 'Admin Panel',
      url: "/admin",
     }
  ];
  activeLink = this.links[0];
  background = '';
  linkUrls = ["/kyc", "/login", "/vote", "/resultsverify", "/admin", "/details/:id"];

  toggleBackground() {
    this.background = this.background ? '' : 'primary';
  }

}
