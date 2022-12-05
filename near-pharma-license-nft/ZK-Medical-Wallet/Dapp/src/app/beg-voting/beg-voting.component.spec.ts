import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { BegVotingComponent } from './beg-voting.component';

describe('BegVotingComponent', () => {
  let component: BegVotingComponent;
  let fixture: ComponentFixture<BegVotingComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ BegVotingComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(BegVotingComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
