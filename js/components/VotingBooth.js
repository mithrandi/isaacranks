import React, {PropTypes as P} from 'react'
import IP from 'react-immutable-proptypes'
import CSSTransitionGroup from 'react-addons-css-transition-group'
import {Row, Col} from 'react-bootstrap'
import VotingPanel from './VotingPanel'
import VotingItemPanel from './VotingItemPanel'

export default class VotingBooth extends React.Component {
  static propTypes =
  { onReroll: P.func.isRequired
  , left: IP.map
  , right: IP.map
  }

  render() {
    const {left, right, onReroll} = this.props
    return (
      <div>
        <Row>
          <CSSTransitionGroup transitionName="voting"
                              transitionEnterTimeout={150}
                              transitionLeave={false}
                              component="div">
            <VotingItemPanel key={left.get('ballot')} ballot={left} />
          </CSSTransitionGroup>
          <VotingPanel bsStyle="info"
                       title="?"
                       label="Reroll!"
                       onVote={onReroll}>
            I don't know / can't decide
          </VotingPanel>
          <CSSTransitionGroup transitionName="voting"
                              transitionEnterTimeout={150}
                              transitionLeave={false}
                              component="div">
            <VotingItemPanel key={right.get('ballot')} ballot={right} />
          </CSSTransitionGroup>
        </Row>
        <Row>
          <Col md={4}>
            <h2>Keyboard bindings:</h2>
            <dl className="dl-horizontal">
              <dt>Left</dt>
              <dd>Vote for the item on the left.</dd>
              <dt>Up</dt>
              <dd>Reroll!</dd>
              <dt>Right</dt>
              <dd>Vote for the item on the right</dd>
            </dl>
          </Col>
        </Row>
      </div>
      )
  }
}
