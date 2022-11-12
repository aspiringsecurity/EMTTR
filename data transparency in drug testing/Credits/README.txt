Improving Data Transparency in Clinical Trials Using Blockchain Smart Contracts
===============================================================================

Paper
=====

https://f1000research.com/articles/5-2541/v1

4th October 2016 

Authors: Timothy Nugent, David Upton and Mihai Cimpoesu

Package contents
================

src/Regulator.sol - the solidity smart contract code for the Regulator contract
src/ClinicalTrial.sol - the solidity code smart contract code for the a Clinical Trial contract
src/start-ethereum-node.sh - script to start a local Ethereum node
src/workflow.js - the main nodejs script that interacts with the blockchain
src/run-workflow.sh - script that will execute the workflow steps
src/read-from-blockchain.sh - script that reads trial data from the blockchain
logs/run-workflow.log - output from running src/run-workflow.sh
logs/read-from-blockchain.log - output from running src/read-from-blockchain.sh

Getting started
===============

This code has been tested on OSX 10.11.5 and Ubuntu 14.04. Code is written using Bash and Javascript - you will need to install nodejs:

https://nodejs.org

We use the testrpc npm module in order to reproduce the exact conditions the workflow scripts need to run the tests. It is important that you reset the blockchain every time you run through these steps. In order to reset, just restart the start-ethereum-node.sh script.

Running the scripts
===================

The steps needed are as follows:

1) Run 'npm install' to install all dependencies mentioned in package.json

2) Install 'testrpc' as a global npm module

npm install -g ethereumjs-testrpc

3) Run the script to start a local Ethereum node

./start-ethereum-node.sh

4) In another terminal, run the script that will execute the workflow steps

./run-workflow.sh

This script will:

	-Deploy a Regulator smart contract
	-Add a CRO/pharma to this contract - in this case Roche
	-Upload the trial protocol (data/TrialProtocol.pdf) to IPFS, running locally
	-Deploy a Clinical Trial smart contract - in this case for Tamiflu, including the IPFS hash linking to the protocol
	-Add 500 subjects to the trial
	-Add 5 data points for each subject

5) Run the script to read data from the blockchain contracts
./read-from-blockchain.sh

...............................
blockNumber= 3005
drug name= Tamiflu
ipfs hash= QmTZKpCqqFcUsmXWzffmLkYNCwrYkTvhkAXdquzNWij1z4
number of subjects= 501
patient ident= s0/1985-04-03
patient ident= s1/2006-4-1
	 data=  44/mg/89/NONE added at 2016-10-04T16:14:49+01:00
	 data=  50/mg/68/NAUSEA added at 2016-10-04T16:14:49+01:00
	 data=  27/mg/44/HEARTBURN added at 2016-10-04T16:14:49+01:00
	 data=  42/mg/33/COMA added at 2016-10-04T16:14:49+01:00
	 data=  96/mg/54/HEADACHE added at 2016-10-04T16:14:49+01:00
patient ident= s2/2007-5-13
	 data=  10/mg/29/COMA added at 2016-10-04T16:14:49+01:00
	 data=  79/mg/44/NAUSEA added at 2016-10-04T16:14:49+01:00
	 data=  53/mg/21/HEADACHE added at 2016-10-04T16:14:49+01:00
...............................

6) Log files for these scripts can be found in the logs directory

Problems
========

Please contact us:

Mihai Cimpoesu mihai.cimpoesu@gmail.com
Timothy Nugent tim.nugent@thomsonreuters.com
David Upton dupton0@gmail.com
