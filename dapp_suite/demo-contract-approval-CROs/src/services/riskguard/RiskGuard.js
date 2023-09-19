import axios from 'axios';

class RiskGuard {
  constructor(user) {
    this.url = `${process.env.REACT_APP_BASE_SERVICE_URL}/mtm-riskguard/api/v1/process`;
    this.user = user;
  }

  static calculateRisk(rgResponse) {
    const extractedSSN = ['**Social Security Numbers:'];
    const extractedCC = ['**Credit Card Numbers:'];
    const extractedBA = ['**Bank Accounts:'];
    const extractedPN = ['**Person Names:'];
    const extractedPhone = ['**Phone Numbers:'];
    const extractedAddress = ['**Addresses:'];
    const extractedGL = ['**Geographic Locations:'];
    const extractedON = ['**Organization Names:'];

    let riskClassification = 1;
    let personNameCount = 0;
    let phoneNumberCount = 0;
    rgResponse.data.results.tme.result.Results.nfinder[0].nfExtract[0].ExtractedTerm.forEach(
      (extractedTerm) => {
        const cartridgeID = extractedTerm.CartridgeID;
        switch (cartridgeID) {
          case 'PN':
            if (extractedTerm.ConfidenceScore > 60) {
              if (extractedTerm.nfinderNormalized) {
                extractedPN.push(extractedTerm.nfinderNormalized);
              } else if (extractedTerm.MainTerm.value) {
                extractedPN.push(extractedTerm.MainTerm.value);
              }
              personNameCount += 1;
            }
            break;
          case 'Phone':
            if (extractedTerm.ConfidenceScore > 60) {
              extractedPhone.push(extractedTerm.nfinderNormalized);
              phoneNumberCount += 1;
            }
            break;
          case 'Address':
            if (extractedTerm.ConfidenceScore > 60) {
              extractedAddress.push(extractedTerm.nfinderNormalized);
            }
            break;
          case 'GL':
            if (extractedTerm.ConfidenceScore > 60) {
              extractedGL.push(extractedTerm.MainTerm.value);
            }
            break;
          case 'ON':
            if (extractedTerm.ConfidenceScore > 60) {
              if (extractedTerm.nfinderNormalized) {
                extractedON.push(extractedTerm.nfinderNormalized);
              } else if (extractedTerm.MainTerm.value) {
                extractedON.push(extractedTerm.MainTerm.value);
              }
            }
            break;
          case 'SSN':
            extractedSSN.push(extractedTerm.ClientNormalized);
            if (riskClassification < 5) {
              riskClassification = 5;
            }
            break;
          case 'CreditCard':
            extractedCC.push(extractedTerm.ClientNormalized);
            if (riskClassification < 4) {
              riskClassification = 4;
            }
            break;
          case 'BankAccount':
            extractedBA.push(extractedTerm.ClientNormalized);
            if (riskClassification < 2) {
              riskClassification = 2;
            }
            break;
          default:
            // Do nothing
        }
        if (riskClassification < 3 && (personNameCount >= 5
          || (personNameCount > 2 && phoneNumberCount > 2))
        ) {
          riskClassification = 3;
        }
      },
    );

    const extractedTerms = [].concat(
      extractedSSN,
      extractedCC,
      extractedBA,
      extractedPN,
      extractedPhone,
      extractedAddress,
      extractedGL,
      extractedON,
    );

    return {
      riskClassification,
      extractedTerms,
    };
  }

  async processDoc(fileData, fileName) {
    const form = new FormData();
    form.append('File', fileData, fileName);
    const postRequest = {
      method: 'post',
      url: this.url,
      headers: {
        Authorization: `Bearer ${this.user.access_token}`,
        'Content-Type': 'multipart/form-data',
      },
      data: form,
    };

    return new Promise((resolve, reject) => {
      axios(postRequest).then((postResponse) => {
        resolve({ data: RiskGuard.calculateRisk(postResponse) });
      }).catch((response) => {
        reject(response.response);
      });
    });
  }
}

export default RiskGuard;
