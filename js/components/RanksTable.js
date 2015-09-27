import React, {PropTypes as P} from 'react'
import {Set} from 'immutable'
import IP from 'react-immutable-proptypes'
import {Table} from 'react-bootstrap'
import Item from './Item'

export default class RanksTable extends React.Component {
  static propTypes =
  { minRating: P.number.isRequired
  , maxRating: P.number.isRequired
  , pools: IP.set.isRequired
  , items: P.arrayOf(P.object).isRequired
  }

  render() {
    const {minRating, maxRating, pools} = this.props
    const ratingRange = maxRating - minRating
    const items = this.props.items.map((item, index) => {
      const norm = (item.rating - minRating) / ratingRange * 1000
      if (Set(item.pools).intersect(pools).size > 0)
        return <Item key={item.isaacId} index={index} item={item} norm={norm} />
    })
    return (
      <Table condensed hover responsive className="ranks-table">
        <thead>
          <tr>
            <th>#</th>
            <th />
            <th>Name</th>
            <th>Description</th>
            <th>Rating</th>
            <th>Normalized</th>
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
