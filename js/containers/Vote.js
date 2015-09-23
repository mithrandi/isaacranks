import React, {PropTypes as P} from 'react'
import IP from 'react-immutable-proptypes'
import {bindActionCreators} from 'redux'
import {connect} from 'react-redux'
import {Map} from 'immutable'
import VotingBooth from '../components/VotingBooth'
import * as VoteActions from '../actions/Vote'

function mapStateToProps(state) {
  return state.voting.toObject()
}

function mapDispatchToProps(dispatch) {
  return {actions: bindActionCreators(VoteActions, dispatch)}
}

@connect(mapStateToProps, mapDispatchToProps)
export default class Vote extends React.Component {
  static propTypes =
  { actions: P.objectOf(P.func).isRequired
  , ballots: P.objectOf(IP.listOf).isRequired
  }

  componentDidMount() {
    this.props.actions.loadBallot(this.props.params.version)
  }

  componentWillReceiveProps(nextProps) {
    nextProps.actions.loadBallot(nextProps.params.version)
  }

  render() {
    const {actions, ballots} = this.props
    const {version} = this.props.params
    const ballot = ballots.getIn([version, 0], Map())
    const left = ballot.get('left', Map()).merge(
      { 'onVote':
        () => actions.voteFor(ballot.get('ballotLeft'))
      , 'ballot': ballot.get('ballotLeft')
      })
    const right = ballot.get('right', Map()).merge(
      { 'onVote':
        () => actions.voteFor(ballot.get('ballotRight'))
      , 'ballot': ballot.get('ballotRight')
      })
    return (
      <div>
        <div className="jumbotron">
          <h1>Rank items</h1>
          <p>Please select the item you would prefer below:</p>
        </div>
        <VotingBooth left={left} right={right} />
      </div>
      )
  }
}
