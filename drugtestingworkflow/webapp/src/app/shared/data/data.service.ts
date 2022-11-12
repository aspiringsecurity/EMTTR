import { Router, Data } from '@angular/router';

import { EventEmitter, Injectable } from "@angular/core";
import { Headers, Http, Response, URLSearchParams, RequestOptions } from "@angular/http";
import { Observable } from 'rxjs/Observable';

import 'rxjs/add/operator/catch';
import 'rxjs/add/operator/map';
import { Subject } from 'rxjs/Subject';


export class Task {

    title: string
    deposit: number
    accession_number: string
    description: string
    smiles: string


    constructor() {
        this.title = ""
        this.deposit = 1

    }
}


export class Study {
    registration_date: Date
    first_enrollment_date: Date
    target_sample_size: number
    updated_date: Date
    summary: string
    conditions: string
    arms: any

    // recruitment info
    status: string
    country_code: string
    completion_date: Date


    constructor() {
        this.target_sample_size = 0
        this.arms = []
        this.status = 'pending'
    }
}


export class Arm {
    intervention_name: string
    type: string
    classification: string
    descriptions: string
    constructor(_intervention_name, _type, _classification, _descriptions) {
        this.intervention_name = _intervention_name;
        this.type = _type;
        this.classification = _classification
        this.descriptions = _descriptions
    }
}




@Injectable()
export class DataService {

    url = "http://178.128.120.220:5984"

    constructor(
        private http: Http
    ) {

    }
    generateGuid() {
        var result, i, j;
        result = '';
        for (j = 0; j < 32; j++) {
            if (j == 8 || j == 12 || j == 16 || j == 20)
                result = result + '-';
            i = Math.floor(Math.random() * 16).toString(16).toUpperCase();
            result = result + i;
        }
        return result;
    }

    getBaseURL() {
        return this.url+"/task/";
    }

    getTokenURI(uri): Observable<any> {
        let headers = new Headers();
        headers.append('Content-Type', 'application/json');
        let options = new RequestOptions({ headers: headers });
        return this.http.get(uri, options)
            .map((res: Response) => res.json())
            .catch(
                (error: Response) => {
                    return Observable.throw(error);
                }
            )

    }

    createTask(id: string, doc: any): Observable<any> {
        let headers = new Headers();
        headers.append('Content-Type', 'application/json');
        let options = new RequestOptions({ headers: headers });
        return this.http.put(this.url + "/task/" + id, doc, options)
            .map((res: Response) => res.json())
            .catch(
                (error: Response) => {
                    return Observable.throw(error);
                }
            )
    }

}

/*
    sample data
    DATABASE_ID	GENERIC_NAME	SMILES
DB00119	Pyruvic acid	CC(=O)C(O)=O
DB00126	Vitamin C	[H][C@@]1(OC(=O)C(O)=C1O)[C@@H](O)CO
DB00128	L-Aspartic Acid	N[C@@H](CC(O)=O)C(O)=O
DB00129	L-Ornithine	NCCC[C@H](N)C(O)=O
DB00131	Adenosine monophosphate	NC1=C2N=CN([C@@H]3O[C@H](COP(O)(O)=O)[C@@H](O)[C@H]3O)C2=NC=N1
DB00139	Succinic acid	OC(=O)CCC(O)=O
DB00158	Folic Acid	NC1=NC(=O)C2=NC(CNC3=CC=C(C=C3)C(=O)N[C@@H](CCC(O)=O)C(O)=O)=CN=C2N1
DB00161	L-Valine	CC(C)[C@H](N)C(O)=O
DB00173	Adenine	NC1=C2NC=NC2=NC=N1
DB00175	Pravastatin	[H][C@]12[C@H](C[C@H](O)C=C1C=C[C@H](C)[C@@H]2CC[C@@H](O)C[C@@H](O)CC(O)=O)OC(=O)[C@@H](C)CC
DB00178	Ramipril	[H][C@@]12CCC[C@]1([H])N([C@@H](C2)C(O)=O)C(=O)[C@H](C)N[C@@H](CCC1=CC=CC=C1)C(=O)OCC
DB00180	Flunisolide	[H][C@@]12C[C@@]3([H])[C@]4([H])C[C@H](F)C5=CC(=O)C=C[C@]5(C)[C@@]4([H])[C@@H](O)C[C@]3(C)[C@@]1(OC(C)(C)O2)C(=O)CO
DB00187	Esmolol	COC(=O)CCC1=CC=C(OCC(O)CNC(C)C)C=C1
DB00192	Indecainide	CC(C)NCCCC1(C(N)=O)C2=CC=CC=C2C2=CC=CC=C12
DB00198	Oseltamivir	CCOC(=O)C1=C[C@@H](OC(CC)CC)[C@H](NC(C)=O)[C@@H](N)C1
DB00208	Ticlopidine	ClC1=CC=CC=C1CN1CCC2=C(C1)C=CS2
DB00209	Trospium	OC(C(=O)OC1CC2CCC(C1)[N+]21CCCC1)(C1=CC=CC=C1)C1=CC=CC=C1
DB00212	Remikiren	CC(C)(C)S(=O)(=O)C[C@H](CC1=CC=CC=C1)C(=O)N[C@@H](CC1=CN=CN1)C(=O)N[C@H](CC1CCCCC1)[C@H](O)[C@H](O)C1CC1
DB00215	Citalopram	CN(C)CCCC1(OCC2=C1C=CC(=C2)C#N)C1=CC=C(F)C=C1
DB00220	Nelfinavir	[H][C@@]12CCCC[C@]1([H])CN(C[C@@H](O)[C@H](CSC1=CC=CC=C1)NC(=O)C1=C(C)C(O)=CC=C1)[C@@H](C2)C(=O)NC(C)(C)C
DB00221	Isoetarine	CCC(NC(C)C)C(O)C1=CC(O)=C(O)C=C1
DB00223	Diflorasone	[H][C@@]12C[C@H](C)[C@](O)(C(=O)CO)[C@@]1(C)C[C@H](O)[C@@]1(F)[C@@]2([H])C[C@H](F)C2=CC(=O)C=C[C@]12C
DB00224	Indinavir	CC(C)(C)NC(=O)[C@@H]1CN(CC2=CN=CC=C2)CCN1C[C@@H](O)C[C@@H](CC1=CC=CC=C1)C(=O)N[C@@H]1[C@H](O)CC2=CC=CC=C12
DB00226	Guanadrel	NC(N)=NCC1COC2(CCCCC2)O1
DB00234	Reboxetine	[H][C@]1(CNCCO1)[C@@H](OC1=CC=CC=C1OCC)C1=CC=CC=C1
DB00240	Alclometasone	[H][C@@]12C[C@@H](C)[C@](O)(C(=O)CO)[C@@]1(C)C[C@H](O)[C@@]1([H])[C@@]2([H])[C@H](Cl)CC2=CC(=O)C=C[C@]12C
DB00243	Ranolazine	COC1=CC=CC=C1OCC(O)CN1CCN(CC(=O)NC2=C(C)C=CC=C2C)CC1
DB00247	Methysergide	[H][C@@]12CC3=CN(C)C4=C3C(=CC=C4)C1=C[C@H](CN2C)C(=O)NC(CC)CO
DB00248	Cabergoline	[H][C@@]12CC3=CNC4=CC=CC(=C34)[C@@]1([H])C[C@H](CN2CC=C)C(=O)N(CCCN(C)C)C(=O)NCC
DB00274	Cefmetazole	[H][C@]12SCC(CSC3=NN=NN3C)=C(N1C(=O)[C@]2(NC(=O)CSCC#N)OC)C(O)=O
DB00282	Pamidronate	NCCC(O)(P(O)(O)=O)P(O)(O)=O
DB00287	Travoprost	CC(C)OC(=O)CCC\C=C/C[C@H]1[C@@H](O)C[C@@H](O)[C@@H]1\C=C\[C@@H](O)COC1=CC=CC(=C1)C(F)(F)F
DB00298	Dapiprazole	CC1=CC=CC=C1N1CCN(CCC2=NN=C3CCCCN23)CC1
DB00307	Bexarotene	CC1=CC2=C(C=C1C(=C)C1=CC=C(C=C1)C(O)=O)C(C)(C)CCC2(C)C
DB00310	Chlorthalidone	NS(=O)(=O)C1=C(Cl)C=CC(=C1)C1(O)NC(=O)C2=CC=CC=C12
DB00312	Pentobarbital	CCCC(C)C1(CC)C(=O)NC(=O)NC1=O
DB00315	Zolmitriptan	CN(C)CCC1=CNC2=CC=C(C[C@H]3COC(=O)N3)C=C12
DB00323	Tolcapone	CC1=CC=C(C=C1)C(=O)C1=CC(=C(O)C(O)=C1)[N+]([O-])=O
DB00324	Fluorometholone	[H][C@@]12CC[C@](O)(C(C)=O)[C@@]1(C)C[C@H](O)[C@@]1(F)[C@@]2([H])C[C@H](C)C2=CC(=O)C=C[C@]12C
DB00331	Metformin	CN(C)C(=N)NC(N)=N
DB00335	Atenolol	CC(C)NCC(O)COC1=CC=C(CC(N)=O)C=C1
*/