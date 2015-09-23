import React, {PropTypes as P} from 'react'
import IP from 'react-immutable-proptypes'
import {bindActionCreators} from 'redux'
import {connect} from 'react-redux'
import {Map} from 'immutable'
import * as VoteActions from '../actions/Vote'
import VotingBooth from '../components/VotingBooth'
import ErrorPage from '../components/ErrorPage'

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
    const left = ballot.get('left', Map()).merge(
      { 'onVote':
        () => actions.voteFor(version, ballot.get('ballotLeft'))
      , 'ballot': ballot.get('ballotLeft')
      })
    const right = ballot.get('right', Map()).merge(
      { 'onVote':
        () => actions.voteFor(version, ballot.get('ballotRight'))
      , 'ballot': ballot.get('ballotRight')
      })
    const onReroll = () => actions.reroll(version)
    return (
      <div>
        <div className="jumbotron">
          <h1>Rank items</h1>
          <p>Please select the item you would prefer below:</p>
        </div>
        <VotingBooth left={left} right={right} onReroll={onReroll} />
      </div>
      )
  }
}
