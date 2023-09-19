import {
  useState,
  useEffect,
  useRef,
} from 'react';
import PropTypes from 'prop-types';
import axios from 'axios';
import { useAuth } from 'react-oidc-context';
import {
  Dialog,
  DialogContent,
} from '@material-ui/core';
import FileViewer from './FileViewer';

const baseUrl = process.env.REACT_APP_BASE_SERVICE_URL;

function DocumentDialogView({ open, fileId, onClose }) {
  const { user } = useAuth();
  const [publicationData, setPublicationData] = useState('');
  const openRef = useRef(open);

  useEffect(() => {
    const getPublicationData = () => {
      axios({
        method: 'get',
        url: `${baseUrl}/cms/instances/file/ca_contract/${fileId}/contents`,
        headers: {
          Authorization: `Bearer ${user.access_token}`,
        },
      }).then((result) => {
        let blobId = '';
        // eslint-disable-next-line no-underscore-dangle
        if (result && result.data && result.data._embedded && result.data._embedded.collection) {
          // eslint-disable-next-line no-underscore-dangle
          result.data._embedded.collection.forEach((content) => {
            if (content.name === 'Brava rendition') {
              blobId = content.blob_id;
            }
          });
        }
        if (blobId) {
          axios({
            method: 'get',
            url: `${process.env.REACT_APP_CSS_SERVICE_URL}/v2/content/${blobId}/download`,
            headers: {
              Authorization: `Bearer ${user.access_token}`,
            },
          }).then((publicationResult) => {
            if (publicationResult.data.status === 'Complete') {
              if (publicationResult) {
                setPublicationData(publicationResult.data);
              }
            }
          }).catch((error) => {
            // eslint-disable-next-line no-alert
            alert(
              error.response != null && error.response.data != null
                ? error.response.data : error.message,
            );
          });
        }
      }).catch((error) => {
        // eslint-disable-next-line no-alert
        alert(
          error.response != null && error.response.data != null
            ? error.response.data : error.message,
        );
      });
    };

    if (openRef.current !== open) {
      openRef.current = open;
      if (open) {
        getPublicationData();
      }
    }
  }, [user.access_token, fileId, open]);

  return (
    <Dialog
      open={open}
      aria-labelledby="customized-dialog-title"
      fullScreen
      maxWidth="md"
    >
      <DialogContent>
        <FileViewer
          closeDialog={onClose}
          publicationData={publicationData}
        />
      </DialogContent>
    </Dialog>
  );
}

DocumentDialogView.propTypes = {
  open: PropTypes.bool.isRequired,
  fileId: PropTypes.string.isRequired,
  onClose: PropTypes.func.isRequired,
};

export default DocumentDialogView;
