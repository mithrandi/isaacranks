import React, {PropTypes as P} from 'react'
import zfill from 'zero-fill'

export default class ItemIcon extends React.Component {
  static propTypes =
  { 'isaacId': P.number.isRequired
  , 'href': P.string.isRequired
  , 'title': P.string.isRequired
  };

  render() {
    const itemId = zfill(3, this.props.isaacId)
    const className = `center-block icons icons-collectibles_${itemId}`
    return (
      <a className={className} href={this.props.href} title={this.props.title} />
      )
  }
}
