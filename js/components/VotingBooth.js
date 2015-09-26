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
            <VotingItemPanel key={left.get('ballot')}
                             ballot={left}
                             hotkeyName="Left"
                             hotkeyIcon="caret-square-o-left" />
          </CSSTransitionGroup>
          <VotingPanel bsStyle="info"
                       title="?"
                       label="Reroll!"
                       onVote={onReroll}
                       hotkeyName="Up"
                       hotkeyIcon="caret-square-o-up">
            I don't know / can't decide
          </VotingPanel>
          <CSSTransitionGroup transitionName="voting"
                              transitionEnterTimeout={150}
                              transitionLeave={false}
                              component="div">
            <VotingItemPanel key={right.get('ballot')}
                             ballot={right}
                             hotkeyName="Right"
                             hotkeyIcon="caret-square-o-right" />
          </CSSTransitionGroup>
        </Row>
      </div>
      )
  }
}
