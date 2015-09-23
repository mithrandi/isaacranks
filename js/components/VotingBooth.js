import React, {PropTypes as P} from 'react'
import IP from 'react-immutable-proptypes'
import CSSTransitionGroup from 'react-addons-css-transition-group'
import {Row, Col, Panel} from 'react-bootstrap'
import VotingPanel from './VotingPanel'
import VotingItemPanel, {BallotType} from './VotingItemPanel'

export default class VotingBooth extends React.Component {
  static propTypes =
  { onReroll: P.func.isRequired
  , left: IP.map
  , right: IP.map
  }

  render() {
    const {left, right, onReroll} = this.props
    return (
      <Row>
        <CSSTransitionGroup transitionName="voting"
                            transitionEnterTimeout={300}
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
                            transitionEnterTimeout={300}
                            transitionLeave={false}
                            component="div">
          <VotingItemPanel key={right.get('ballot')} ballot={right} />
        </CSSTransitionGroup>
      </Row>
      )
  }
}
