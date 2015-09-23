import React, {PropTypes as P} from 'react'
import {Button} from 'react-bootstrap'

export default class ErrorPage extends React.Component {
  static propTypes =
  { 'error': P.string.isRequired
  , 'onReset': P.func.isRequired
  }

  render () {
    return (
      <div>
        <div className="jumbotron">
          <h1>Oops!</h1>
          <p>An error occurred:</p>
        </div>
        <p>{this.props.children}</p>
        <p><Button onClick={this.props.onReset}>Reset</Button></p>
      </div>)
  }
}
