import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { NewStudyComponent } from './new-study.component';

describe('NewStudyComponent', () => {
  let component: NewStudyComponent;
  let fixture: ComponentFixture<NewStudyComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ NewStudyComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(NewStudyComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
