import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { TabNavBarBasicExampleComponent } from './tab-nav-bar-basic-example.component';

describe('TabNavBarBasicExampleComponent', () => {
  let component: TabNavBarBasicExampleComponent;
  let fixture: ComponentFixture<TabNavBarBasicExampleComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ TabNavBarBasicExampleComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(TabNavBarBasicExampleComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
