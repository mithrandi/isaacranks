import React, {PropTypes as P} from 'react'
import {bindActionCreators} from 'redux'
import {connect} from 'react-redux'
import Filters from '../components/Filters'
import RanksTable from '../components/RanksTable'
import * as RanksActions from '../actions/Ranks'


class Ranks extends React.Component {
  render() {
    if (this.props.loading || this.props.error) {
      const message = this.props.loading ? 'Loading...' : this.props.error
      return (
        <div className="jumbotron">
          <h1>Item ranks</h1>
          <p>{message}</p>
        </div>
      )
    }
    const {items, votesCast, meanVotes, minRating, maxRating} = this.props.ranks
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
Ranks.propTypes =
{ actions: P.objectOf(P.func).isRequired
, loading: P.bool.isRequired
, error: P.string
, ranks: P.shape(
  { votesCast: P.number.isRequired
  , meanVotes: P.number.isRequired
  , minRating: P.number.isRequired
  , maxRating: P.number.isRequired
  , items: P.arrayOf(
    P.shape(
      { isaacId: P.number.isRequired
      , imageId: P.string.isRequired
      , name: P.string.isRequired
      , description: P.string.isRequired
      , wiki: P.string.isRequired
      , rating: P.number.isRequired
      , votes: P.number.isRequired
      , pools: P.arrayOf(P.string.isRequired).isRequired
      }))
  })
}

function mapStateToProps(state) {
  return state.ranks
}

function mapDispatchToProps(dispatch) {
  return {actions: bindActionCreators(RanksActions, dispatch)}
}

export default connect(mapStateToProps, mapDispatchToProps)(Ranks)
