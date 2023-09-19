import {
  useCallback,
  useEffect,
  useRef,
  useState,
} from 'react';
import axios from 'axios';
import { useAuth } from 'react-oidc-context';
import {
  Backdrop,
  Button,
  CircularProgress,
  IconButton,
  Paper,
  Snackbar,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
} from '@material-ui/core';
import ArrowForwardIosIcon from '@material-ui/icons/ArrowForwardIos';
import AddIcon from '@material-ui/icons/Add';
import CloseIcon from '@material-ui/icons/Close';
import Alert from './Alert';
import ContractDetails from './ContractDetails';
import AddContract from './AddContract';
import DocumentDialogView from './DocumentDialogView';
import Pagination from './Pagination';
import RiskClassification from './RiskClassification';

const baseUrl = process.env.REACT_APP_BASE_SERVICE_URL;

/**
 * This view displays the list of created contracts.
 * From here the user can request approval for any of them.
 */
function CreatedContractList() {
  const { user } = useAuth();
  const [state, setState] = useState(
    {
      contracts: [],
      lineManagerAclId: '',
      riskManagerAclId: '',
      completedAclId: '',
      openContractDetails: false,
      selectedContract: { properties: {} },
      openAddContract: false,
      addNumberOfContracts: 0,
      pageNumber: 0,
      count: -1,
      openDocumentDialogView: false,
      fileId: '',
      fileName: '',
      showBackdrop: false,
      showSnackBar: false,
      snackBarMessage: '',
      snackBarSeverity: 'success',
    },
  );
  const didMountRef = useRef(false);
  const addNumberOfContractsRef = useRef(state.addNumberOfContracts);
  const pageNumberRef = useRef(state.pageNumber);

  const raiseError = useCallback((errorMessage) => {
    setState((prevState) => ({
      ...prevState,
      snackBarSeverity: 'error',
      snackBarMessage: errorMessage,
      showSnackBar: true,
    }));
  }, []);

  const getAcls = useCallback(() => {
    if (
      state.lineManagerAclId.length === 0
      || state.riskManagerAclId.length === 0
      || state.completedAclId.length === 0) {
      let lineManagerAclFound = false;
      let riskManagerAclFound = false;
      let completedAclFound = false;
      axios({
        method: 'get',
        url: `${baseUrl}/cms/permissions?filter=name eq "line_manager_approval" or name eq "risk_manager_approval" or name eq "completed"`,
        headers: {
          Authorization: `Bearer ${user.access_token}`,
        },
      }).then((res) => {
        // eslint-disable-next-line no-underscore-dangle
        const acls = res.data._embedded.collection;
        acls.forEach((acl) => {
          switch (acl.name) {
            case 'line_manager_approval':
              setState((prevState) => ({ ...prevState, lineManagerAclId: acl.id }));
              lineManagerAclFound = true;
              break;
            case 'risk_manager_approval':
              setState((prevState) => ({ ...prevState, riskManagerAclId: acl.id }));
              riskManagerAclFound = true;
              break;
            case 'completed':
              setState((prevState) => ({ ...prevState, completedAclId: acl.id }));
              completedAclFound = true;
              break;
            default:
          }
        });

        if (!lineManagerAclFound
          || !riskManagerAclFound
          || !completedAclFound) {
          const errorMessage = 'Not all required ACLs exist in the repository';
          raiseError(errorMessage);
        }
      }).catch((error) => {
        let errorMessage = 'Could not get ACLs: ';
        if (error.response != null && error.response.data != null) {
          errorMessage += error.response.data.exception;
        } else {
          errorMessage += error.message;
        }
        raiseError(errorMessage);
      });
    }
  }, [raiseError, state, user.access_token]);

  const getContracts = useCallback(() => {
    if (user.profile.preferred_username) {
      setState((prevState) => ({ ...prevState, showBackdrop: true }));
      axios({
        method: 'get',
        url: `${baseUrl}/cms/instances/file/ca_contract/?include-total=true&sortby=create_time desc&filter=status eq "CREATED"&page=${state.pageNumber + 1}`,
        headers: {
          Authorization: `Bearer ${user.access_token}`,
        },
      }).then((res) => {
        setState((prevState) => ({
          ...prevState,
          // eslint-disable-next-line no-underscore-dangle
          contracts: res.data && res.data._embedded ? res.data._embedded.collection : [],
          count: res.data.total,
        }));
      }).catch((error) => {
        let errorMessage = 'Could not get contracts: ';
        if (error.response != null && error.response.data != null) {
          errorMessage += error.response.data.exception;
        } else {
          errorMessage += error.message;
        }
        raiseError(this, errorMessage);
      }).finally(() => {
        setState((prevState) => ({ ...prevState, showBackdrop: false }));
      });
    } else {
      setState((prevState) => ({ ...prevState, contracts: [], count: -1 }));
    }
  }, [
    raiseError,
    user.access_token,
    user.profile.preferred_username,
    state.pageNumber,
  ]);

  const handleContractAdded = (errorMessage) => {
    if (!errorMessage) {
      setState((prevState) => ({
        ...prevState,
        addNumberOfContracts: state.addNumberOfContracts + 1,
        snackBarSeverity: 'success',
        snackBarMessage: 'Contract added successfully',
      }));
    } else {
      setState((prevState) => ({
        ...prevState,
        snackBarSeverity: 'error',
        snackBarMessage: errorMessage,
      }));
    }
    setState((prevState) => ({ ...prevState, showSnackBar: true }));
  };

  const handleCloseAddContract = () => {
    setState((prevState) => ({ ...prevState, openAddContract: false }));
  };

  const handleCloseContractDetails = () => {
    setState((prevState) => ({ ...prevState, openContractDetails: false }));
  };

  const handleCloseDocumentDialogView = () => {
    setState((prevState) => ({ ...prevState, openDocumentDialogView: false }));
  };

  const handleSnackBarClose = () => {
    setState((prevState) => ({ ...prevState, showSnackBar: false }));
  };

  const handlePageNumber = (pageNumber) => {
    setState((prevState) => ({ ...prevState, pageNumber }));
  };

  const openDocumentDialogView = (fileId, fileName) => {
    setState((prevState) => ({
      ...prevState,
      openDocumentDialogView: true,
      fileId,
      fileName,
    }));
  };

  const startContractForApproval = (contractId) => {
    setState((prevState) => ({ ...prevState, showBackdrop: true }));

    const data = {
      processDefinitionKey: 'contract_approval',
      name: 'Approve contract',
      outcome: 'none',
      variables: [
        {
          name: 'base_url',
          value: baseUrl,
        },
        {
          name: 'contract_id',
          value: contractId,
        },
        {
          name: 'line_manager_approval_acl_id',
          value: state.lineManagerAclId,
        },
        {
          name: 'risk_manager_approval_acl_id',
          value: state.riskManagerAclId,
        },
        {
          name: 'completed_acl_id',
          value: state.completedAclId,
        },
      ],
      returnVariables: true,
    };

    axios({
      method: 'post',
      url: `${baseUrl}/workflow/v1/process-instances`,
      headers: {
        Authorization: `Bearer ${user.access_token}`,
      },
      data,
    }).then(() => {
      setState((prevState) => ({ ...prevState, snackBarMessage: 'Approval requested successfully.' }));
      setState((prevState) => ({ ...prevState, showSnackBar: true }));
      getContracts();
    }).catch((error) => {
      const statusCode = error.response.status;
      let errorMessage = 'Error requesting approval: ';
      if (statusCode === 400) {
        errorMessage += error.response.data.exception;
      } else {
        errorMessage += error.message;
      }
      raiseError(errorMessage);
    }).finally(() => {
      setState((prevState) => ({ ...prevState, showBackdrop: false }));
    });
  };

  const showDetails = (contract) => {
    setState((prevState) => ({
      ...prevState,
      selectedContract: contract,
      openContractDetails: true,
    }));
  };

  const getDateValue = (dt) => (dt ? new Date(Date.parse(dt)).toLocaleString() : '');

  const openContractDialog = () => {
    setState((prevState) => ({ ...prevState, openAddContract: true }));
  };

  useEffect(() => {
    if (didMountRef.current) {
      if (addNumberOfContractsRef.current !== state.addNumberOfContracts
        || pageNumberRef.current !== state.pageNumber
      ) {
        getContracts();
        addNumberOfContractsRef.current = state.addNumberOfContracts;
        pageNumberRef.current = state.pageNumber;
      }
    } else {
      getAcls();
      getContracts();
      didMountRef.current = true;
    }
  }, [state.addNumberOfContracts, state.pageNumber, getAcls, getContracts]);

  return (
    <div>
      <Button variant="contained" color="primary" disabled={!user.profile.preferred_username} startIcon={<AddIcon />} onClick={() => openContractDialog()} style={{ margin: '0.25rem' }}>Add</Button>
      <div className="content-header">All created contracts</div>
      <TableContainer component={Paper}>
        <Table size="small" aria-label="a dense table">
          <TableHead>
            <TableRow>
              <TableCell>Contract name</TableCell>
              <TableCell align="left">Creation date</TableCell>
              <TableCell align="left">Value</TableCell>
              <TableCell align="left">Risk classification</TableCell>
              <TableCell align="left">View document</TableCell>
              <TableCell align="left">Action</TableCell>
              <TableCell align="left" />
            </TableRow>
          </TableHead>
          <TableBody>
            {state.contracts.map((row) => (
              <TableRow key={row.id}>
                <TableCell component="th" scope="row">
                  {row.name}
                </TableCell>
                <TableCell align="left">{getDateValue(row.create_time)}</TableCell>
                <TableCell align="left">{row.properties.value}</TableCell>
                <TableCell align="left"><RiskClassification row={row} /></TableCell>
                <TableCell align="left">
                  <Button size="small" variant="outlined" color="primary" onClick={() => openDocumentDialogView(row.id, row.name)}>Original</Button>
                </TableCell>
                <TableCell align="left">
                  <Button size="small" variant="outlined" color="primary" onClick={() => startContractForApproval(row.id)}>Request approval</Button>
                </TableCell>
                <TableCell align="left">
                  <IconButton size="small" variant="outlined" color="primary" title="Show details" onClick={() => showDetails(row)}>
                    <ArrowForwardIosIcon />
                  </IconButton>
                </TableCell>
              </TableRow>
            ))}
          </TableBody>
        </Table>
      </TableContainer>
      <Pagination
        pageNumber={state.pageNumber}
        count={state.count}
        handlePageNumber={handlePageNumber}
      />
      <ContractDetails
        open={state.openContractDetails}
        selectedContract={state.selectedContract}
        parentRaiseError={raiseError}
        onClose={handleCloseContractDetails}
      />
      <AddContract
        open={state.openAddContract}
        onAddContract={handleContractAdded}
        onClose={handleCloseAddContract}
      />
      <DocumentDialogView
        open={state.openDocumentDialogView}
        fileId={state.fileId}
        onClose={handleCloseDocumentDialogView}
      />
      <Backdrop style={{ zIndex: 9999 }} open={state.showBackdrop}>
        <CircularProgress color="inherit" />
      </Backdrop>
      <Snackbar
        anchorOrigin={{
          vertical: 'bottom',
          horizontal: 'center',
        }}
        open={state.showSnackBar}
        autoHideDuration={5000}
        onClose={handleSnackBarClose}
        action={
          (
            <IconButton size="small" aria-label="close" color="inherit" onClick={handleSnackBarClose}>
              <CloseIcon fontSize="small" />
            </IconButton>
          )
        }
      >
        <Alert onClose={handleSnackBarClose} severity={state.snackBarSeverity}>
          {state.snackBarMessage}
        </Alert>
      </Snackbar>
    </div>
  );
}

export default CreatedContractList;
