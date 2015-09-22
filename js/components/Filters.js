import React, {PropTypes as P} from 'react'
import IP from 'react-immutable-proptypes'
import {Button, ButtonToolbar, ButtonGroup} from 'react-bootstrap'
import FilterButton from './FilterButton'
import poolNames from '../constants/pools'

export default class Filters extends React.Component {
  static propTypes =
  { pools: IP.set.isRequired
  , onToggle: P.func.isRequired
  , onAll: P.func.isRequired
  , onNone: P.func.isRequired
  }

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
