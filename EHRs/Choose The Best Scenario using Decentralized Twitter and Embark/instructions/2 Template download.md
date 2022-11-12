# DTwitter Template clone and explore
Now that we have Embark installed properly, let’s grab the DTwitter dApp template that we will use as the skeleton of our dApp. This template is a website built using React (don't worry if you don't know React, we are not focussing on this part). 

## We will...
We will leverage Embark’s featureset to write our contract, write unit tests for the contract, and then use these functions inside the website.

## Let's go
Clone the DTwitter template into your dApp folder:
```
cd [parent folder] (ie ~/dev/__github)
git clone https://github.com/status-im/dappcon-workshop-dapp
cd dappcon-workshop-dapp
git checkout start-here
```
Let’s take a moment to look at the file structure of the dApp and open the template in our IDE.
* `/app` - contains all our assets for the website. These will get webpacked according to our settings in `/embark.json`.
* `/config` - contains all our configuration
    * Blockchain - configures options for running geth
    * Communication - configures whisper options
    * Contracts - configures options for deploying contracts from Embark, as well as the connection to make to geth from the dApp
    * Namesystem - configures ENS support (coming in 3.2)
    * Storage - configures decentralised storage for IPFS and Swarm. Includes a section for uploading the dApp as well as a dApp connection that can be used in the dApp.
    * Webserver - configuration options for the webserver that serves the dApp during development.
* `/contracts` - contains our contacts
* `/test` - contains our mocha tests for testing our contracts
* `embark.json` - configures file locations, webpack options, and library versions to use