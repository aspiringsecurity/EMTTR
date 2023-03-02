import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { CryptonComponent } from './crypton.component';

describe('CryptonComponent', () => {
  let component: CryptonComponent;
  let fixture: ComponentFixture<CryptonComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ CryptonComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(CryptonComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
