import React, {PropTypes as P} from 'react'
import {bindActionCreators} from 'redux'
import {connect} from 'react-redux'
import {Jumbotron} from 'react-bootstrap'
import Filters from '../components/Filters'
import RanksTable from '../components/RanksTable'
import ErrorAlert from '../components/ErrorAlert'
import * as RanksActions from '../actions/Ranks'
import {RanksState} from '../reducers/ranks'

function mapStateToProps(state) {
  return {ranks: state.ranks}
}

function mapDispatchToProps(dispatch) {
  return {actions: bindActionCreators(RanksActions, dispatch)}
}

@connect(mapStateToProps, mapDispatchToProps)
export default class Ranks extends React.Component {
  static propTypes =
  { actions: P.objectOf(P.func).isRequired
  , ranks: P.instanceOf(RanksState).isRequired
  };

  componentDidMount() {
    this.props.actions.loadRanks(this.props.match.params.version)
  }

  componentWillReceiveProps(nextProps) {
    nextProps.actions.loadRanks(nextProps.match.params.version)
  }

  render() {
    const {actions} = this.props
    const {version} = this.props.match.params
    const ranks = this.props.ranks.ranks.get(version)
    if (ranks === undefined || ranks.get('loading')) {
      return (
        <Jumbotron>
          <h1>Item ranks</h1>
          <p><i className="fa fa-refresh fa-spin" /></p>
        </Jumbotron>
      )
    } else if (ranks.get('error')) {
      return (
        <div>
          <Jumbotron>
            <h1>Item ranks</h1>
          </Jumbotron>
          <ErrorAlert title="Unable to fetch ranks:" error={ranks.get('error')} onReset={() => actions.resetRanks(version)} />
        </div>
        )
    }
    const {items, votesCast, meanVotes, latestDump} = ranks
    return (
      <div>
        <Jumbotron>
          <h1>Item ranks</h1>
          <p>{votesCast} votes total, mean of {meanVotes.toFixed(2)} per item.</p>
        </Jumbotron>
        <Filters filters={this.props.ranks.filters} onToggle={actions.toggleFilter} onAll={actions.allFilters} onNone={actions.noFilters} />
        <RanksTable items={items} filters={this.props.ranks.filters} />
      </div>
      )
  }
}
