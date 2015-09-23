import React, {PropTypes as P} from 'react'
import {Col, Panel, Button} from 'react-bootstrap'

export default class VotingPanel extends React.Component {
  static propTypes =
  { 'href': P.string
  , 'title': P.string.isRequired
  , 'bsStyle': P.string
  , 'onVote': P.func
  , 'children': P.node
  }

  render() {
    const link = this.props.href
               ? (<a href={this.props.href}>{this.props.title}</a>) : this.props.title
    const header = (<h2 className="panel-title">{link}</h2>)
    const footer = this.props.onVote
                 ? (<Button onClick={this.props.onVote}>Choose</Button>) : null
    return (
      <Col md={4}>
        <Panel bsStyle={this.props.bsStyle}
               className="text-center"
               header={header}
               footer={footer}>
          <p>{this.props.children}</p>
        </Panel>
      </Col>
      )
  }
}
