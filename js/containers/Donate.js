import React from 'react'
import ReactDOM from 'react-dom'
import {Jumbotron, ListGroup, ListGroupItem} from 'react-bootstrap'
import $script from 'scriptjs'

export default class Donate extends React.Component {
  state = {flattrLoaded: false}

  componentDidMount() {
    const node = ReactDOM.findDOMNode(this.refs.flattr)
    $script('https://api.flattr.com/js/0.6/load.js?mode=manual', () => {
      window.FlattrLoader.render(
        { 'button': 'compact'
        , 'uid': 'mithrandi'
        , 'url': 'http://www.isaacranks.com/'
        , 'title': 'Isaac item ranks'
        , 'description': 'A website for ranking items in The Binding of Isaac: Rebirth'
        , 'category': 'software'
        , 'language': 'en_GB'
        }, node, 'replace')
    })
  }

  render() {
    return (
      <div>
        <Jumbotron>
          <h1>Support this site</h1>
          <p>If you would like to support this site, there are a few ways you can do this below</p>
        </Jumbotron>
        <ListGroup>
          <ListGroupItem>
            <div ref="flattr" />
          </ListGroupItem>
          <ListGroupItem>
            <i className="fa fa-btc" />itcoin: 1MrTPdKdAowGMsnVZRs25UhkpHPor28zfm
          </ListGroupItem>
          <ListGroupItem>
            <form action="https://www.paypal.com/cgi-bin/webscr" method="post" target="_top">
              <input type="hidden" name="cmd" value="_s-xclick" />
              <input type="hidden" name="hosted_button_id" value="9JA3FKVCR2DWS" />
              <input type="image" src="https://www.paypalobjects.com/en_US/i/btn/btn_donate_SM.gif" border="0" name="submit" alt="PayPal - The safer, easier way to pay online!" />
            </form>
          </ListGroupItem>
        </ListGroup>
      </div>
      )
  }
}
