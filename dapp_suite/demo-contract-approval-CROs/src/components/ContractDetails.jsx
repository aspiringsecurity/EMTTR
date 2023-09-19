import {
  useEffect,
  useRef,
  useState,
} from 'react';
import PropTypes from 'prop-types';
import axios from 'axios';
import { useAuth } from 'react-oidc-context';
import {
  Button,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  Tab,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  Tabs,
  TextField,
} from '@material-ui/core';
import TabPanel from './TabPanel';

const baseUrl = process.env.REACT_APP_BASE_SERVICE_URL;

function ContractDetails({
  open,
  selectedContract,
  parentRaiseError,
  onClose,
}) {
  const { user } = useAuth();
  const [value, setValue] = useState(0);
  const [contract, setContract] = useState({
    name: '',
    properties: {},
    traits: {},
    create_time: '',
  });
  const [contractAcl, setContractAcl] = useState();
  const openRef = useRef(open);
  const selectedContractRef = useRef(selectedContract);

  const getApprovals = (trait) => {
    const approvals = [];

    Object.keys(trait).forEach((key) => {
      const approval = trait[key];
      approval.trait_name = key;
      approvals.push(approval);
    });

    return approvals.sort((a, b) => (a.trait_name < b.trait_name ? -1 : 1));
  };

  const ensureNonNullStringValue = (propertyValue) => {
    let stringValue;

    if (typeof propertyValue === 'boolean') {
      stringValue = propertyValue.toString();
    } else if (propertyValue) {
      stringValue = propertyValue;
    } else {
      stringValue = '';
    }

    return stringValue;
  };

  const handleChange = (event, newValue) => {
    setValue(newValue);
  };

  const getDateValue = (dt) => (dt ? new Date(Date.parse(dt)).toLocaleString() : '');

  useEffect(() => {
    const raiseError = (errorMessage) => {
      onClose();
      parentRaiseError(errorMessage);
    };

    const fetchSelectedContract = async () => {
      await axios({
        method: 'get',
        url: `${baseUrl}/cms/instances/file/${selectedContract.type}/${selectedContract.id}`,
        headers: {
          Authorization: `Bearer ${user.access_token}`,
        },
      }).then((res) => {
        setContract(res.data);
      }).catch((error) => {
        let errorMessage = `Could not fetch ${selectedContract.type} with Id ${selectedContract.id}: `;
        if (error.response != null && error.response.data != null) {
          errorMessage += error.response.data.exception;
        } else {
          errorMessage += error.message;
        }
        raiseError(errorMessage);
      });
    };

    const fetchAclForSelectedContract = async () => {
      await axios({
        method: 'get',
        url: `${baseUrl}/cms/instances/file/${selectedContract.type}/${selectedContract.id}/acl`,
        headers: {
          Authorization: `Bearer ${user.access_token}`,
        },
      }).then((res) => {
        setContractAcl(res.data);
      }).catch((error) => {
        let errorMessage = `Could not fetch ACL for ${selectedContract.type} with Id ${selectedContract.id}: `;
        if (error.response != null && error.response.data != null) {
          errorMessage += error.response.data.exception;
        } else {
          errorMessage += error.message;
        }
        raiseError(errorMessage);
      });
    };

    if (openRef.current !== open || selectedContractRef.current !== selectedContract) {
      openRef.current = open;
      selectedContractRef.current = selectedContract;
      setContract({});
      fetchSelectedContract();
      fetchAclForSelectedContract();
    }
  }, [open, selectedContract, user.access_token, onClose, parentRaiseError]);

  return (
    <div>
      { contract.id !== undefined
        && (
        <Dialog open={open} aria-labelledby="form-dialog-title">
          <DialogTitle id="form-dialog-title">Contract details</DialogTitle>
          <DialogContent className="contract-details">
            <Tabs orientation="horizontal" value={value} onChange={handleChange}>
              <Tab className="tab-caption" label="Properties" />
              <Tab className="tab-caption" label="Permissions" />
              <Tab className="tab-caption" label="Approvals" />
            </Tabs>
            <TabPanel value={value} index={0}>
              <TextField
                margin="dense"
                id="contract-name"
                label="Name"
                value={contract.name}
                type="text"
                fullWidth
              />
              <TextField
                margin="dense"
                id="contract-status"
                label="Status"
                value={contract.properties.status}
                type="text"
                fullWidth
              />
              <TextField
                margin="dense"
                id="contract-value"
                label="Value"
                value={contract.properties.value}
                type="text"
                fullWidth
              />
              {
                contract.type === 'ca_loan_contract'
                && (
                <>
                  <TextField
                    margin="dense"
                    id="monthly-installments"
                    label="Monthly installments"
                    value={contract.properties.monthly_installments}
                    type="text"
                    fullWidth
                  />
                  <TextField
                    margin="dense"
                    id="yearly-income"
                    label="Yearly income"
                    value={contract.properties.yearly_income}
                    type="text"
                    fullWidth
                  />
                </>
                )
              }
              <TextField
                margin="dense"
                id="contract-risk"
                label="Risk classification"
                value={contract.properties.risk_classification}
                type="text"
                fullWidth
              />
              <TextField
                margin="dense"
                id="createTime"
                label="Creation date"
                value={getDateValue(contract.create_time)}
                type="text"
                InputLabelProps={{ shrink: true }}
                fullWidth
              />
              <TextField
                margin="dense"
                id="contract-requester-email"
                label="Contract requester email"
                value={contract.properties.requester_email}
                type="text"
                fullWidth
              />
            </TabPanel>
            <TabPanel value={value} index={1}>
              {
                contractAcl?.permits
                && (
                  <TableContainer>
                    <Table>
                      <TableHead>
                        <TableRow>
                          <TableCell align="left">Identity</TableCell>
                          <TableCell align="left">Permissions</TableCell>
                        </TableRow>
                      </TableHead>
                      <TableBody>
                        {contractAcl.permits
                          .map((permit) => ({ ...permit, display_name: (ensureNonNullStringValue(permit.identity_type) === 'subscription_group' ? permit.group_name : permit.identity) }))
                          .sort(
                            (permita, permitb) => (
                              permita.display_name > permitb.display_name ? 1 : -1
                            ),
                          )
                          .map((permit) => (
                            <TableRow key={permit.identity}>
                              <TableCell align="left" style={{ verticalAlign: 'top' }}>{permit.display_name}</TableCell>
                              <TableCell align="left" style={{ verticalAlign: 'top' }}>
                                {permit.permissions.sort(
                                  (permissiona, permissionb) => (
                                    permissiona > permissionb ? 1 : -1
                                  ),
                                ).map((permission) => (
                                  <li key={permission}>{permission}</li>
                                ))}
                              </TableCell>
                            </TableRow>
                          ))}
                      </TableBody>
                    </Table>
                  </TableContainer>
                )
              }
            </TabPanel>
            <TabPanel value={value} index={2}>
              <TableContainer>
                <Table>
                  <TableHead>
                    <TableRow>
                      <TableCell />
                      <TableCell align="right">Required</TableCell>
                      <TableCell align="right">Granted</TableCell>
                      <TableCell align="right">Approver</TableCell>
                      <TableCell align="right">Approver role</TableCell>
                      <TableCell align="right">Approval date</TableCell>
                    </TableRow>
                  </TableHead>
                  <TableBody>
                    {getApprovals(contract.traits.ca_approval).map((approval) => (
                      <TableRow key={approval.trait_name}>
                        <TableCell component="th" scope="row">{ensureNonNullStringValue(approval.trait_name)}</TableCell>
                        <TableCell align="right">{ensureNonNullStringValue(approval.is_required)}</TableCell>
                        <TableCell align="right">{ensureNonNullStringValue(approval.has_been_granted)}</TableCell>
                        <TableCell align="right">{ensureNonNullStringValue(approval.approver)}</TableCell>
                        <TableCell align="right">{ensureNonNullStringValue(approval.approver_role)}</TableCell>
                        <TableCell align="right">{ensureNonNullStringValue(approval.approval_date)}</TableCell>
                      </TableRow>
                    ))}
                  </TableBody>
                </Table>
              </TableContainer>
            </TabPanel>
          </DialogContent>
          <DialogActions>
            <Button onClick={() => onClose()} variant="contained" color="primary">
              Close
            </Button>
          </DialogActions>
        </Dialog>
        )}
    </div>
  );
}

ContractDetails.propTypes = {
  open: PropTypes.bool.isRequired,
  selectedContract: PropTypes.shape({
    id: PropTypes.string,
    type: PropTypes.string,
  }).isRequired,
  parentRaiseError: PropTypes.func.isRequired,
  onClose: PropTypes.func.isRequired,
};

export default ContractDetails;
