import React, { Component } from 'react';
import {Grid, Row, Col, PageHeader} from 'react-bootstrap';

/**
 * Class representing the home page rendering
 * 
 * @extends React.Component
 */
class Home extends Component{

  //#region Constructor
  constructor(props){
    super(props);
  }
  //#endregion

  //#region React lifecycle events
  render() {
    return (
      <Grid>
        <Row>
          <Col xs={12}>
            <PageHeader>
              Decentralised Twitter <small>Built using Embark by Status</small>
            </PageHeader>
          </Col>
        </Row>
      </Grid>
    );
  }
  //#endregion
}

export default Home