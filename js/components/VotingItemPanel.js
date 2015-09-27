import React, {PropTypes as P} from 'react'
import IP from 'react-immutable-proptypes'
import VotingPanel from './VotingPanel'
import zfill from 'zero-fill'

export default class VotingItemPanel extends React.Component {
  static propTypes =
  { 'ballot': IP.map
  , 'hotkeyName': P.string
  , 'hotkeyIcon': P.string
  }

  render() {
    const {ballot} = this.props
    if (!ballot.get('ballot'))
      return (
        <VotingPanel bsStyle="primary" title="...">
          <i className="fa fa-refresh fa-spin" />
        </VotingPanel>
        )
    const itemId = zfill(3, ballot.get('isaacId'))
    const className = `center-block icons icons-collectibles_${itemId}`
    return (
      <VotingPanel bsStyle="primary"
                   href={ballot.get('wiki')}
                   title={ballot.get('name')}
                   label="Choose"
                   onVote={ballot.get('onVote')}
                   hotkeyName={this.props.hotkeyName}
                   hotkeyIcon={this.props.hotkeyIcon}>
        <a className={className} href={ballot.get('wiki')} />
        "{ballot.get('description')}"
      </VotingPanel>
      )
  }
}
