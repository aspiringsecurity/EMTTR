import { NgModule } from '@angular/core';
import { CommonModule } from "@angular/common";

import { FullPagesRoutingModule } from "./full-pages-routing.module";

import { FullLayoutPageComponent } from './full-layout-page.component';

@NgModule({
    imports: [
        CommonModule,
        FullPagesRoutingModule   
    ],
    declarations: [       
        FullLayoutPageComponent
    ]
})
export class FullPagesModule { }
