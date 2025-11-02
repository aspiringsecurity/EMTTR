# 🧬 OP Medicine (EMTTR): Filecoin + Optimism for Decentralized Clinical Trials & Healthcare Data

> **Mission:** Building open developer tools to make **personalized medicine affordable and accessible** — powered by **Filecoin**, **IPFS**, and **Optimism**.

---

## 🌍 Overview

**OP Medicine (EMTTR)** — *Electronic Medicine Trial and Test Records as a Service* — provides decentralized infrastructure for **secure, transparent clinical trials**, **medical billing**, and **EHR/Radiology data management**.

We combine **Filecoin’s verifiable storage layer** with **Optimism’s scalable L2 execution** to deliver:

* Persistent and censorship-resistant health data
* ZK-enabled transparency in drug testing
* Incentivized research collaboration through tokenized utilities (PPT & MED)
* Open-source developer tooling for healthcare organizations

🔗 **Website:** [op-medicine-deploy.vercel.app](https://op-medicine-deploy.vercel.app/)

🏛️ **Civic Portal:** [emttrservice (Google Sites)](https://sites.google.com/view/emttrservice/)

📦 **Invoice Portal (Filecoin Mainnet):** [invoice-ppt-subscribe-storacha-storage.vercel.app](https://invoice-ppt-subscribe-storacha-storage.vercel.app/)

📊 **EMTTR Dashboard:** [emttr-deploy.vercel.app](https://emttr-deploy.vercel.app/)

🔗 **OP Medicine + Py-libp2p Development:** [Website](https://op-medicine-pylibp2p.vercel.app/)

---

## 🧠 Core Architecture

### 🪶 Filecoin/IPFS: Verifiable Storage Layer

* **Immutable data availability** for medical trials, invoices, and diagnostic records
* **Long-term storage on Filecoin** with verifiable persistence and redundancy
* **IPFS integration** for distributed access and off-chain metadata indexing
* **DataDAO modules** on FVM for DICOM and metadata provenance

  * Repo: [fvm-dicom-data-dao](https://github.com/aspiringsecurity/EMTTR/tree/main/fvm-dicom-data-dao)

### ⚡ Optimism: Execution & Incentive Layer

* Smart contracts for **clinical trial workflow**, **medical invoices**, and **attestations**
* **PPT Token** powers micro-payments, audit trails, and staking

  * [Optimism Mainnet Contract](https://optimistic.etherscan.io/address/0xa9c14d3e8ece4d924a4a4a819088f982b55f6734)
* **Optimism NFTs** for anonymized DICOM images and research licensing
* **Free transaction tooling** (OpenGSN + Lightlink) for medical volunteers

### 🧩 Developer Tooling

* **ZK Proofs (Circom/Noir)** for anonymized health data verification
* **Chainlink VRF** for randomized testing and parametric insurance
* **Tableland + IPFS** for searchable metadata and relational notes
* **OpenText APIs** for verifiable medical document signing and CRO workflows
* **Blockscout + Tenderly Integration** for OP transaction tracing and analytics

---

## 🧾 Filecoin Mainnet Deployments

| Module                             | Contract                                     | Filfox                                                                            |
| ---------------------------------- | -------------------------------------------- | --------------------------------------------------------------------------------- |
| **PPT Token**                      | `0xC00BBC9A2C88712dC1e094866973F036373C7134` | [View](https://filfox.info/en/address/0xC00BBC9A2C88712dC1e094866973F036373C7134) |
| **Medical Invoice v1**             | `0x08bacb51f405a2D793E4F4BE53Ca2B3C8b8cF0CA` | [View](https://filfox.info/en/address/0x08bacb51f405a2D793E4F4BE53Ca2B3C8b8cF0CA) |
| **Medical Invoice + Subscription** | `0xb0Bda1Ad964a55ACB077587e42BDfeC587D7e520` | [View](https://filfox.info/en/address/0xb0Bda1Ad964a55ACB077587e42BDfeC587D7e520) |

📍 [Invoice Deployment (Filecoin Mainnet)](https://invoice-ppt-subscribe-storacha-storage.vercel.app/)

---

## 🧬 Ecosystem Modules

### 🧠 FVM Medicine Suite

* Composable imaging and record management for clinical trials
* NFT tags for pharma licenses and DataDAO operations
* Repo: [nfc-tag-nft-fvm](https://github.com/aspiringsecurity/EMTTR/tree/main/EHRs/dicom-optimism-marketplace/nfc-tag-nft-fvm)

### 🩻 OP DICOM Marketplace

* NFT marketplace for radiology images and diagnostics on **Optimism**
* Enables research access with transparent ownership and provenance
* Repo: [dicom-optimism-marketplace](https://github.com/aspiringsecurity/EMTTR/tree/main/EHRs/dicom-optimism-marketplace)

### 🧰 OP TestXLS & OP MED Tools

* Low-code frameworks for visit logs, diagnosis, and medication management
* Deployed on **Optimism** with **Filecoin data anchoring**
* [OP TestXLS](https://github.com/aspiringsecurity/EMTTR/tree/main/EHRs/dicom-optimism-marketplace/OP-testxls)
* [OP MED](https://github.com/aspiringsecurity/EMTTR/tree/main/EHRs/free-OP-transactions-data-volunteers/OP-xls)

---

## 🏆 Achievements & Recognition

* 🪙 **Optimism RetroPGF 2 Nominee** — *Tooling & Utilities Segment*
  [Optimism Mirror](https://optimism.mirror.xyz/Upn_LtV2-3SviXgX_PE_LyA7YI00jQyoM1yf55ltvvI?rpgf=2)
* 🧾 **Filecoin Prize Winner** — *Chainlink Fall Hackathon 2022*
* 🧠 **EVM Ideathon Runner-Up** — *GovTech & Healthcare Public Goods*
* 🏅 **HealthChainhack Boston** — Runner-up
* 🧬 **Young Scientist Award** — India International Science Festival

🎥 [Demo Videos & Decks](https://drive.google.com/drive/u/4/folders/1Mlcb3gKfyNxYpmgCkIo8ZvJCdJuQ8T0a)

---

## 📊 Impact & Public Good Alignment

✅ **Filecoin RetroPGF Alignment:**

* Open-source developer tooling for **FVM + Optimism interoperability**
* Public-good infrastructure for healthcare transparency and auditability
* DataDAO-driven clinical research records, verifiable on-chain

✅ **Optimism RetroPGF Alignment:**

* Low-cost microtransactions for medical research and patient data sharing
* Grants & Incentive flow through transparent smart contracts

---

## 🧩 Developer Setup

```bash
# Clone the repository
git clone https://github.com/aspiringsecurity/EMTTR.git
cd EMTTR

# Install dependencies
npm install

# Start local node and run workflow
./start-ethereum-node.sh
./run-workflow.sh
```

Includes smart contracts for:

* `Regulator.sol`
* `ClinicalTrial.sol`
* `Invoice.sol`
* IPFS upload scripts (`workflow.js`, `read-from-blockchain.sh`)

---

## 💡 Vision

> **From trial to treatment — verified, decentralized, and patient-owned.**
> OP Medicine aims to build a **trusted global infrastructure** for clinical research and healthcare systems — combining **Filecoin’s verifiable storage** with **Optimism’s scalable public-good layer** to enable transparent, affordable, and equitable medicine for all.


