import { useContext, useRef, useState } from 'react';
import PropTypes from 'prop-types';
import axios from 'axios';
import { useAuth } from 'react-oidc-context';
import {
  Button,
  Backdrop,
  CircularProgress,
  TextField,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  FormControlLabel,
  InputLabel,
  MenuItem,
  Radio,
  RadioGroup,
  Select,
  FormControl,
} from '@material-ui/core';
import ApplicationContext from '../context/ApplicationContext';
import RiskGuard from '../services/riskguard/RiskGuard';

const baseUrl = process.env.REACT_APP_BASE_SERVICE_URL;

function AddContract({
  open,
  onAddContract,
  onClose,
}) {
  const { user } = useAuth();
  const { appRootFolderId, updateAppRootFolderId } = useContext(ApplicationContext);
  const [state, setState] = useState(
    {
      showBackdrop: false,
      selectedContract: {
        newContractType: '',
        newContractName: '',
        newContractValue: '',
        newContractRequesterEmail: '',
        newContractMonthlyInstallments: '',
        newContractYearlyIncome: '',
        selectedFile: '',
      },
      riskGuard: {
        contractRisk: '',
        extractedTerms: '',
      },
    },
  );
  const fileNameELementRef = useRef();

  const riskGuardService = new RiskGuard(user);

  let tempAppRootFolderId = appRootFolderId;

  const closeDialog = () => {
    setState((prevState) => ({
      ...prevState,
      newContractType: '',
      newContractName: '',
      newContractValue: '',
      newContractRequesterEmail: '',
      newContractMonthlyInstallments: '',
      newContractYearlyIncome: '',
      showBackdrop: false,
    }));
    onClose();
  };

  const setFileNameInputRef = (element) => {
    fileNameELementRef.current = element;
  };

  const selectFile = (event) => {
    const selectedFile = event.target.files[0];
    setState((prevState) => ({ ...prevState, selectedFile }));
    fileNameELementRef.current.innerHTML = selectedFile.name;
    if (!/\.pdf$/.test(selectedFile.name)) {
      fileNameELementRef.current.innerHTML += '<br/><b>Note: This application only supports pdf files.</b>';
    }
  };

  const handleChangeContractType = (event) => {
    setState((prevState) => ({
      ...prevState,
      newContractType: event.target.value,
      newContractMonthlyInstallments: '',
      newContractYearlyIncome: '',
    }));
  };

  const handleChangeContractName = (event) => {
    setState((prevState) => ({ ...prevState, newContractName: event.target.value }));
  };

  const handleChangeContractValue = (event) => {
    setState((prevState) => ({ ...prevState, newContractValue: event.target.value }));
  };

  const handleChangeContractRequesterEmail = (event) => {
    setState((prevState) => ({ ...prevState, newContractRequesterEmail: event.target.value }));
  };

  const handleChangeContractMonthlyInstallments = (event) => {
    setState((prevState) => ({ ...prevState, newContractMonthlyInstallments: event.target.value }));
  };

  const handleChangeContractYearlyIncome = (event) => {
    setState((prevState) => ({ ...prevState, newContractYearlyIncome: event.target.value }));
  };

  const isLoanContract = () => state.newContractType === 'loan-contract';

  const canSubmit = () => (
    state.newContractName
    && state.newContractValue > 0
    && state.selectedFile
    && state.newContractRequesterEmail
    && (
      !isLoanContract() || (
        state.newContractMonthlyInstallments > 0
        && state.newContractYearlyIncome > 0
      )
    )
  );

  const submitContract = async () => {
    setState((prevState) => ({ ...prevState, showBackdrop: true }));
    const risk = await riskGuardService.processDoc(
      state.selectedFile,
      state.selectedFile.name,
    );
    setState((prevState) => ({
      ...prevState,
      contractRisk: risk.data.riskClassification,
      extractedTerms: risk.data.extractedTerms,
    }));

    // Check for application root folder, and if not existing, create it
    if (tempAppRootFolderId === '') {
      await axios.get(
        `${baseUrl}/cms/instances/folder/cms_folder?filter=name eq 'Contract Approval App'`,
        {
          headers: {
            Authorization: `Bearer ${user.access_token}`,
          },
        },
      ).then((res) => {
        // eslint-disable-next-line no-underscore-dangle
        if (res.data._embedded) {
          // eslint-disable-next-line no-underscore-dangle
          tempAppRootFolderId = res.data._embedded.collection[0].id;
          updateAppRootFolderId(tempAppRootFolderId);
        }
      });
    }

    if (tempAppRootFolderId === '') {
      await axios.post(
        `${baseUrl}/cms/instances/folder/cms_folder`,
        {
          name: 'Contract Approval App',
        },
        {
          headers: {
            Authorization: `Bearer ${user.access_token}`,
          },
        },
      ).then((res) => {
        if (res.data) {
          tempAppRootFolderId = res.data.id;
          updateAppRootFolderId(tempAppRootFolderId);
        }
      }).catch((error) => {
        const statusCode = error.response.status;
        let errorMessage;
        if (statusCode === 401) {
          // Unauthorized access
          errorMessage = 'Error creating App Root Folder: You are not authorized to access this resource. Your session might have timed out.';
        } else {
          errorMessage = `Error creating App Root Folder: ${JSON.stringify(error.response.data, null, 2)}`;
        }
        onAddContract(`Error creating contract: ${errorMessage}`);
      });
    }

    // Check for customer folder, and if not existing, create it
    const customerEmail = state.newContractRequesterEmail;

    let parentFolderId = '';
    await axios.get(
      `${baseUrl}/cms/instances/folder/ca_customer?filter=parent_folder_id eq '${tempAppRootFolderId}' and name eq '${encodeURIComponent(customerEmail)}'`,
      {
        headers: {
          Authorization: `Bearer ${user.access_token}`,
        },
      },
    ).then((res) => {
      // eslint-disable-next-line no-underscore-dangle
      if (res.data._embedded) {
        // eslint-disable-next-line no-underscore-dangle
        parentFolderId = res.data._embedded.collection[0].id;
      }
    });

    if (parentFolderId === '') {
      await axios.post(
        `${baseUrl}/cms/instances/folder/ca_customer`,
        {
          name: customerEmail,
          parent_folder_id: tempAppRootFolderId,
          properties: {
            customer_email: customerEmail,
          },
        },
        {
          headers: {
            Authorization: `Bearer ${user.access_token}`,
          },
        },
      ).then((res) => {
        if (res.data) {
          parentFolderId = res.data.id;
        }
      }).catch((error) => {
        const statusCode = error.response.status;
        let errorMessage;
        if (statusCode === 401) {
          // Unauthorized access
          errorMessage = 'Error creating Customer Folder: You are not authorized to access this resource. Your session might have timed out.';
        } else {
          errorMessage = `Error creating Customer Folder: ${JSON.stringify(error.response.data, null, 2)}`;
        }
        onAddContract(`Error creating contract: ${errorMessage}`);
      });
    }

    // Get the ID of the 'Created' ACL
    let aclId = '';
    await axios.get(
      `${baseUrl}/cms/permissions?filter=name eq 'created'`,
      {
        headers: {
          Authorization: `Bearer ${user.access_token}`,
        },
      },
    ).then((res) => {
      // eslint-disable-next-line no-underscore-dangle
      if (res.data._embedded) {
        // eslint-disable-next-line no-underscore-dangle
        aclId = res.data._embedded.collection[0].id;
      }
    });

    // Adding Contract
    const formData = new FormData();
    formData.append(
      'file',
      state.selectedFile,
      state.selectedFile.name,
    );
    axios.post(
      `${process.env.REACT_APP_CSS_SERVICE_URL}/v2/tenant/${process.env.REACT_APP_TENANT_ID}/content`,
      formData,
      {
        headers: {
          'Content-Type': 'multipart/form-data',
          Authorization: `Bearer ${user.access_token}`,
        },
      },
    ).then((res) => {
      let cmsType;
      if (isLoanContract()) {
        cmsType = 'ca_loan_contract';
      } else {
        cmsType = 'ca_contract';
      }

      // Setting metadata
      return axios({
        method: 'post',
        url: `${baseUrl}/cms/instances/file/${cmsType}`,
        headers: {
          Authorization: `Bearer ${user.access_token}`,
        },
        data: {
          name: state.newContractName,
          parent_folder_id: parentFolderId,
          acl_id: aclId,
          renditions: [
            {
              name: res.data.entries[0].fileName,
              rendition_type: 'primary',
              blob_id: res.data.entries[0].id,
            },
            {
              name: 'Brava rendition',
              mime_type: 'application/vnd.blazon+json',
              rendition_type: 'SECONDARY',
            },
          ],
          properties: {
            value: parseInt(state.newContractValue, 10),
            status: 'CREATED',
            requester_email: customerEmail,
            risk_classification: risk.data.riskClassification,
            extracted_terms: risk.data.extractedTerms,
            ...isLoanContract() && {
              monthly_installments: parseInt(state.newContractMonthlyInstallments, 10),
              yearly_income: parseInt(state.newContractYearlyIncome, 10),
            },
          },
          traits: {
            ca_approval: {
              'Automatic Approval': {
                is_required: true,
                has_been_granted: false,
                approver: '',
                approver_role: '',
              },
              'Line Manager Approval': {
                is_required: false,
                has_been_granted: false,
                approver: '',
                approver_role: '',
              },
              'Risk Manager Approval': {
                is_required: false,
                has_been_granted: false,
                approver: '',
                approver_role: '',
              },
              ...isLoanContract() && {
                'Solvency Check': {
                  is_required: true,
                  has_been_granted: false,
                  approver: '',
                  approver_role: '',
                },
              },
            },
          },
        },
      });
    }).then(() => {
      closeDialog();
      onAddContract();
    }).catch((error) => {
      const statusCode = error.response.status;
      let errorMessage;
      if (statusCode === 400) {
        // Validation error
        errorMessage = 'The parameter name is mandatory';
      } else if (statusCode === 401) {
        // Unauthorized access
        errorMessage = 'You are not authorized to access this resource. Your session might have timed out.';
      } else {
        errorMessage = JSON.stringify(error.response.data, null, 2);
      }
      onAddContract(`Error creating contract: ${errorMessage}`);
    })
      .finally(() => {
        setState((prevState) => ({ ...prevState, showBackdrop: false }));
      });
  };

  return (
    <Dialog open={open} aria-labelledby="form-dialog-title">
      <DialogTitle id="form-dialog-title">Add Contract</DialogTitle>
      <DialogContent className="add-contract">
        <div>
          <div className="inline">
            <label htmlFor="files" className="MuiButtonBase-root MuiButton-root MuiButton-contained MuiButton-containedPrimary">
              Select Document
              <input id="files" type="file" accept="application/pdf" className="file-input" onChange={selectFile} />
            </label>
          </div>
          <div id="fileName" className="inline margin-start" ref={setFileNameInputRef} />
        </div>
        <br />
        <RadioGroup
          row
          defaultValue="standard-contract"
          name="contract-types-radio-buttons-group"
          onChange={handleChangeContractType}
        >
          <FormControlLabel value="standard-contract" control={<Radio />} label="Standard Contract" size="small" />
          <FormControlLabel value="loan-contract" control={<Radio />} label="Loan Contract" />
        </RadioGroup>
        <TextField
          margin="dense"
          id="contract-name"
          label="Document name"
          type="text"
          defaultValue=""
          fullWidth
          onChange={handleChangeContractName}

        />
        <TextField
          margin="dense"
          id="contract-value"
          label="Contract value"
          type="number"
          InputProps={{ inputProps: { min: 1 } }}
          fullWidth
          onChange={handleChangeContractValue}

        />
        {
          isLoanContract()
          && (
            <>
              <FormControl fullWidth>
                <InputLabel id="contract-monthly-installments-label">Monthly installments</InputLabel>
                <Select
                  margin="dense"
                  labelId="contract-monthly-installments-label"
                  id="contract-monthly-installments"
                  label="Monthly installments"
                  type="number"
                  defaultValue=""
                  onChange={handleChangeContractMonthlyInstallments}
                >
                  <MenuItem value={12}>12</MenuItem>
                  <MenuItem value={24}>24</MenuItem>
                  <MenuItem value={36}>36</MenuItem>
                  <MenuItem value={48}>48</MenuItem>
                  <MenuItem value={60}>60</MenuItem>
                  <MenuItem value={72}>72</MenuItem>
                  <MenuItem value={84}>84</MenuItem>
                </Select>
              </FormControl>
              <TextField
                margin="dense"
                id="contract-yearly-income"
                label="Yearly income"
                type="number"
                defaultValue=""
                InputProps={{ inputProps: { min: 1 } }}
                fullWidth
                onChange={handleChangeContractYearlyIncome}
              />
            </>
          )
        }
        <TextField
          margin="dense"
          id="contract-requester-email"
          label="Contract requester email"
          type="text"
          defaultValue=""
          fullWidth
          onChange={handleChangeContractRequesterEmail}

        />
      </DialogContent>
      <DialogActions>
        <Button
          onClick={() => submitContract()}
          variant="contained"
          color="primary"
          disabled={!canSubmit()}
        >
          Add
        </Button>
        <Button onClick={() => closeDialog()} color="primary">
          Cancel
        </Button>
      </DialogActions>
      <Backdrop style={{ zIndex: 9999 }} open={state.showBackdrop}>
        <CircularProgress color="inherit" />
      </Backdrop>
    </Dialog>
  );
}

AddContract.propTypes = {
  open: PropTypes.bool.isRequired,
  onAddContract: PropTypes.func.isRequired,
  onClose: PropTypes.func.isRequired,
};

export default AddContract;
