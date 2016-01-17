import React, {PropTypes as P} from 'react'
import {Button, Alert} from 'react-bootstrap'

export default class ErrorAlert extends React.Component {
  static propTypes =
  { 'title': P.string.isRequired
  , 'error': P.string.isRequired
  , 'onReset': P.func.isRequired
  };

  render () {
    return (
      <Alert bsStyle="danger">
        <h2>{this.props.title}</h2>
        <p>{this.props.error.toString()}</p>
        <p><Button bsStyle="danger" onClick={this.props.onReset}>Retry</Button></p>
      </Alert>
      )
  }
}
