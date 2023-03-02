import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { KycVerifierComponent } from './kyc-verifier.component';

describe('KycVerifierComponent', () => {
  let component: KycVerifierComponent;
  let fixture: ComponentFixture<KycVerifierComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ KycVerifierComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(KycVerifierComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
