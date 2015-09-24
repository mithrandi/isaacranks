import React, {PropTypes as P} from 'react'
import IP from 'react-immutable-proptypes'
import {bindActionCreators} from 'redux'
import {connect} from 'react-redux'
import {Map} from 'immutable'
import {HotKeys} from 'react-hotkeys'
import * as VoteActions from '../actions/Vote'
import VotingBooth from '../components/VotingBooth'
import ErrorPage from '../components/ErrorPage'

function keyPressOnce(component, name, handler) {
  const down = (event) => {
    event.preventDefault()
    if (!component.state.pressed.get(name, false)) {
      component.setState(
        {pressed: component.state.pressed.set(name, true)})
      handler()
    }
  }
  const up = (event) => {
    event.preventDefault()
    component.setState(
      {pressed: component.state.pressed.set(name, false)})
  }
  return {[name + 'Down']: down, [name + 'Up']: up}
}

function mapStateToProps(state) {
  return {voting: state.voting}
}

function mapDispatchToProps(dispatch) {
  return {actions: bindActionCreators(VoteActions, dispatch)}
}

@connect(mapStateToProps, mapDispatchToProps)
export default class Vote extends React.Component {
  static propTypes =
  { actions: P.objectOf(P.func).isRequired
  , voting: IP.map.isRequired
  }

  state = {pressed: Map()}

  componentDidMount() {
    this.props.actions.loadBallot(this.props.params.version)
  }

  componentWillReceiveProps(nextProps) {
    nextProps.actions.loadBallot(nextProps.params.version)
  }

  render() {
    const {actions, voting} = this.props
    const error = voting.get('error')
    if (error) {
      return (<ErrorPage onReset={actions.reset}>{error}</ErrorPage>)
    }

    const {version} = this.props.params
    const ballot = voting.getIn([version, 'ballots', 0], Map())
    const ballotLeft = ballot.get('ballotLeft')
    const ballotRight = ballot.get('ballotRight')
    const onVoteLeft = () => actions.voteFor(version, ballotLeft)
    const onVoteRight = () => actions.voteFor(version, ballotRight)
    const left = ballot.get('left', Map()).merge(
      { 'onVote': onVoteLeft
      , 'ballot': ballotLeft
      })
    const right = ballot.get('right', Map()).merge(
      { 'onVote': onVoteRight
      , 'ballot': ballotRight
      })
    const onReroll = () => actions.reroll(version)

    const keyMap =
    { 'voteLeftDown': {sequence: 'left', action: 'keydown'}
    , 'voteLeftUp': {sequence: 'left', action: 'keyup'}
    , 'rerollDown': {sequence: 'up', action: 'keydown'}
    , 'rerollUp': {sequence: 'up', action: 'keyup'}
    , 'voteRightDown': {sequence: 'right', action: 'keydown'}
    , 'voteRightUp': {sequence: 'right', action: 'keyup'}
    }
    const handlers = !ballotLeft ? {} : Object.assign(
      {},
      keyPressOnce(this, 'voteLeft', onVoteLeft),
      keyPressOnce(this, 'reroll', onReroll),
      keyPressOnce(this, 'voteRight', onVoteRight))

    return (
      <HotKeys keyMap={keyMap} handlers={handlers} focused={true} attach={window}>
        <div className="jumbotron">
          <h1>Rank items</h1>
          <p>Please select the item you would prefer below:</p>
        </div>
        <VotingBooth left={left} right={right} onReroll={onReroll} />
      </HotKeys>
      )
  }
}
