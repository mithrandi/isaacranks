import React, {PropTypes as P} from 'react'
import {Set} from 'immutable'
import IP from 'react-immutable-proptypes'
import {Table} from 'react-bootstrap'
import Item from './Item'
import {POOL, TYPE} from '../constants/filters'

export default class RanksTable extends React.Component {
  static propTypes =
  { minRating: P.number.isRequired
  , maxRating: P.number.isRequired
  , filters: IP.map.isRequired
  , items: P.arrayOf(P.object).isRequired
  };

  render() {
    const {filters} = this.props
    const pools = filters.get(POOL)
    const types = filters.get(TYPE)
    const items = this.props.items.map((item, index) => {
      if (Set(item.pools).add('SPECIAL').intersect(pools).size > 0 && types.has(item.itype))
        return <Item key={item.isaacId} index={index} item={item} />
    })
    return (
      <Table condensed hover responsive className="ranks-table">
        <thead>
          <tr>
            <th>#</th>
            <th />
            <th>Name</th>
            <th>Description</th>
            <th>Votes</th>
          </tr>
        </thead>
        <tbody>
          {items}
        </tbody>
      </Table>
    )
  }
}
