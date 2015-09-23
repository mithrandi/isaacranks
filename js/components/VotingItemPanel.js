import React from 'react'
import IP from 'react-immutable-proptypes'
import VotingPanel from './VotingPanel'

export default class VotingItemPanel extends React.Component {
  static propTypes = {'ballot': IP.map}

  render() {
    const {ballot} = this.props;
    if (!ballot.get('ballot'))
      return (
        <VotingPanel bsStyle="primary" title="...">
          <i className="fa fa-refresh fa-spin" />
        </VotingPanel>
        )
    const className = `center-block rebirth-item r-itm${ballot.get('imageId')}`
    return (
      <VotingPanel bsStyle="primary"
                   href={ballot.get('wiki')}
                   title={ballot.get('name')}
                   onVote={ballot.get('onVote')}>
        <a className={className} href={ballot.get('wiki')} />
        {ballot.get('description')}
      </VotingPanel>
      )
  }
}
