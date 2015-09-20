import React, {PropTypes as P} from 'react'
import IP from 'react-immutable-proptypes'
import {Button, ButtonToolbar, ButtonGroup} from 'react-bootstrap'
import FilterButton from './FilterButton'
import poolNames from '../constants/pools'

class Filters extends React.Component {
  render() {
    const {pools, onToggle, onAll, onNone} = this.props
    const buttons = poolNames.map(
      ([name, label]) =>
        <FilterButton name={name} key={name} label={label} pools={pools} onToggle={onToggle} />)
    return (
      <ButtonToolbar>
        <ButtonGroup>
          {buttons}
        </ButtonGroup>
        <ButtonGroup>
          <Button onClick={onAll}>All</Button>
          <Button onClick={onNone}>None</Button>
        </ButtonGroup>
      </ButtonToolbar>
    )
  }
}

Filters.propTypes =
{ pools: IP.set.isRequired
, onToggle: P.func.isRequired
, onAll: P.func.isRequired
, onNone: P.func.isRequired
}

export default Filters
