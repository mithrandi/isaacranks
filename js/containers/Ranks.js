import React, {PropTypes as P} from 'react'
import {bindActionCreators} from 'redux'
import {connect} from 'react-redux'
import Filters from '../components/Filters'
import RanksTable from '../components/RanksTable'
import * as RanksActions from '../actions/Ranks'


function mapStateToProps(state) {
  return state.ranks
}

function mapDispatchToProps(dispatch) {
  return {actions: bindActionCreators(RanksActions, dispatch)}
}

@connect(mapStateToProps, mapDispatchToProps)
export default class Ranks extends React.Component {
  static propTypes =
  { actions: P.objectOf(P.func).isRequired
  , ranks: P.object
  }

  componentDidMount() {
    this.props.actions.loadRanks(this.props.params.version)
  }

  componentWillReceiveProps(nextProps) {
    nextProps.actions.loadRanks(nextProps.params.version)
  }

  render() {
    const ranks = this.props.ranks[this.props.params.version]
    if (ranks === undefined || ranks.loading || ranks.error) {
      const message = ranks === undefined || ranks.loading ? 'Loading...' : ranks.error
      return (
        <div className="jumbotron">
          <h1>Item ranks</h1>
          <p>{message}</p>
        </div>
      )
    }
    const {items, votesCast, meanVotes, minRating, maxRating} = ranks
    const {actions} = this.props
    return (
      <div>
        <div className="jumbotron">
          <h1>Item ranks</h1>
          <p>{votesCast} votes total, mean of {meanVotes.toFixed(2)} per item.</p>
          <p>Normalized rating is normalized to range [0, 1000].</p>
        </div>
        <Filters pools={this.props.pools} onToggle={actions.togglePool} onAll={actions.allPools} onNone={actions.noPools} />
        <RanksTable items={items} minRating={minRating} maxRating={maxRating} pools={this.props.pools} />
      </div>
    )
  }
}
