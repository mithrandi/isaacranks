import React, {PropTypes as P} from 'react'
import IP from 'react-immutable-proptypes'
import {Button} from 'react-bootstrap'

export default class FilterButton extends React.Component {
  static propTypes =
  { name: P.string.isRequired
  , pools: IP.setOf(P.string).isRequired
  , label: P.string.isRequired
  , onToggle: P.func.isRequired
  }

  render() {
    const {name, pools, label, onToggle} = this.props
    const bsStyle = pools.has(name) ? 'success' : 'danger';
    return <Button bsStyle={bsStyle} onClick={() => onToggle(name)}>{label}</Button>
  }
}
