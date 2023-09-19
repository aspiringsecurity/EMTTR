import PropTypes from 'prop-types';
import {
  Button,
  TextField,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
} from '@material-ui/core';

function TaskDetails({ open, selectedTask, onClose }) {
  const closeDialog = () => onClose();

  const getDateValue = (dt) => (dt ? new Date(Date.parse(dt)).toLocaleString() : '');

  const getContractName = (task) => {
    if (task && task.variables[0]) {
      return task.variables.find((q) => q.name === 'contract').value.name;
    }
    return '';
  };

  const getRequesterEmail = (task) => {
    if (task && task.variables[0]) {
      return task.variables.find((q) => q.name === 'contract').value.properties.requester_email;
    }
    return '';
  };

  const getContractValue = (task) => {
    if (task && task.variables[0]) {
      return task.variables.find((q) => q.name === 'contract').value.properties.value;
    }
    return '';
  };

  return (
    <Dialog open={open} aria-labelledby="form-dialog-title">
      <DialogTitle id="form-dialog-title">Task details</DialogTitle>
      <DialogContent>
        <TextField
          margin="dense"
          id="name"
          label="Task name"
          value={selectedTask.name}
          type="text"
          fullWidth
        />
        <TextField
          margin="dense"
          id="assignee"
          label="Assignee"
          value={selectedTask.assignee || ''}
          type="text"
          fullWidth
        />
        <TextField
          margin="dense"
          id="createTime"
          label="Creation date"
          value={getDateValue(selectedTask.createTime)}
          type="text"
          InputLabelProps={{
            shrink: true,
          }}
          fullWidth
        />
        <TextField
          margin="dense"
          id="cmsName"
          label="Contract name"
          value={getContractName(selectedTask)}
          type="text"
          fullWidth
        />
        <TextField
          margin="dense"
          id="cmsEmail"
          label="Requester email address"
          value={getRequesterEmail(selectedTask)}
          type="text"
          fullWidth
        />
        <TextField
          margin="dense"
          id="cmsValue"
          label="Contract value"
          value={getContractValue(selectedTask)}
          type="text"
          fullWidth
        />
      </DialogContent>
      <DialogActions>
        <Button onClick={() => closeDialog()} variant="contained" color="primary">
          Close
        </Button>
      </DialogActions>
    </Dialog>
  );
}

TaskDetails.propTypes = {
  open: PropTypes.bool.isRequired,
  selectedTask: PropTypes.shape({
    name: PropTypes.string,
    assignee: PropTypes.string,
    createTime: PropTypes.string,
    variables: PropTypes.arrayOf(
      PropTypes.shape({
        // eslint-disable-next-line react/forbid-prop-types
        value: PropTypes.any.isRequired,
      }),
    ),
  }).isRequired,
  onClose: PropTypes.func.isRequired,
};

export default TaskDetails;
