import React, {PropTypes as P} from 'react'
import IP from 'react-immutable-proptypes'
import {Button} from 'react-bootstrap'

export default class FilterButton extends React.Component {
  static propTypes =
  { name: P.string.isRequired
  , activeFilters: IP.setOf(P.string).isRequired
  , label: P.string.isRequired
  , onToggle: P.func.isRequired
  };

  render() {
    const {name, activeFilters, label, onToggle} = this.props
    const active = activeFilters.has(name) ? true : false
    return <Button bsSize="small" active={active} onClick={() => onToggle(name)}>{label}</Button>
  }
}
