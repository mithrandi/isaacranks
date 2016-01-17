import React, {PropTypes as P} from 'react'
import IP from 'react-immutable-proptypes'
import {Button, ButtonToolbar, ButtonGroup} from 'react-bootstrap'
import FilterButton from './FilterButton'
import allFilters from '../constants/filters'


const FilterGroup = (props) => {
  return (
    <ButtonGroup>
      {props.allFilters.map(([name, label]) =>
         <FilterButton name={name}
                       key={name}
                       label={label}
                       activeFilters={props.activeFilters}
                       onToggle={props.onToggle} />)}
    </ButtonGroup>
    )
}


export default class Filters extends React.Component {
  static propTypes =
  { filters: IP.map.isRequired
  , onToggle: P.func.isRequired
  , onAll: P.func.isRequired
  , onNone: P.func.isRequired
  };

  render() {
    const {filters, onToggle, onAll, onNone} = this.props
    const filterGroups = allFilters.entrySeq().map(
      ([ft, af]) =>
      <FilterGroup key={ft.toString()}
                   allFilters={af}
                   activeFilters={filters.get(ft)}
                   onToggle={(name) => onToggle(ft, name)} />).toArray()
    return (
      <ButtonToolbar>
        {filterGroups}
        <ButtonGroup>
          <Button bsSize="small" onClick={onAll}>All</Button>
          <Button bsSize="small" onClick={onNone}>None</Button>
        </ButtonGroup>
      </ButtonToolbar>
    )
  }
}
