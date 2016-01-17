import React, {PropTypes as P} from 'react'
import {Col, Panel, Button, OverlayTrigger, Tooltip} from 'react-bootstrap'

const HotkeyOverlay = (props) => {
  if (props.icon) {
    const iconClass = `fa fa-lg fa-${props.icon}`
    const overlay = (
      <Tooltip id="keyboard-binding">
        Keyboard binding: <i className={iconClass} aria-hidden={true} />
        <span className="sr-only">{props.name}</span>
      </Tooltip>)
    return (
      <OverlayTrigger placement="bottom" overlay={overlay}>
        {props.children}
      </OverlayTrigger>
      )
  } else {
    return props.children
  }
}

export default class VotingPanel extends React.Component {
  static propTypes =
  { 'href': P.string
  , 'title': P.string.isRequired
  , 'bsStyle': P.string
  , 'label': P.string
  , 'onVote': P.func
  , 'children': P.node
  , 'hotkeyName': P.string
  , 'hotkeyIcon': P.string
  };

  render() {
    const link = this.props.href
               ? (<a href={this.props.href}>{this.props.title}</a>) : this.props.title
    const header = (<h2 className="panel-title">{link}</h2>)
    const footer = this.props.onVote ? (
      <HotkeyOverlay icon={this.props.hotkeyIcon} name={this.props.hotkeyName}>
        <Button onClick={this.props.onVote}>{this.props.label}</Button>
      </HotkeyOverlay>
      ) : null
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
