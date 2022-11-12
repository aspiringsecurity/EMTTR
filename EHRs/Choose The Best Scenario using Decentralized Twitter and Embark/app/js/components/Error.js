import React, { Component } from 'react';
import { Grid, Row, Col, PageHeader } from 'react-bootstrap';

/**
 * Class representing the home page rendering
 * 
 * @extends React.Component
 */
class Error extends Component {

  //#region Constructor
  constructor(props) {
    super(props);
  }
  //#endregion

  //#region React lifecycle events
  render() {

    const metaMaskPossible = (this.props.error.message.indexOf('Internal JSON-RPC error') > 0 || this.props.error.message.indexOf('Failed to fetch') > 0);

    return (
      <Grid>
        <Row>
          <Col xs={12}>
            <PageHeader>
              Whoopsieee <small>Something went wrong</small>
            </PageHeader>
            {metaMaskPossible ?
              <React.Fragment>
                <h3>Metamask error?</h3>
                <p>It appears you might be using metamask. Have you signed in and are you on the right network?</p>
              </React.Fragment>
              :
              ''
            }
            <h3>Error details</h3>
            <pre>{this.props.error.message}<br />
            {this.props.error.stack}
            { this.props.error.source ? 
              <React.Fragment>
                <br/>Source: { this.props.error.source }
              </React.Fragment>
              : 
              ''
            }</pre>
          </Col>
        </Row>
      </Grid>
    );
  }
  //#endregion
}

export default Error