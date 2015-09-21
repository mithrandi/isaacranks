import React, {Component, PropTypes} from 'react'
import {connect} from 'react-redux'
import {Link} from 'react-router'

class App extends Component {
  render() {
    return (
      <div>
        <nav className="navbar navbar-inverse navbar-fixed-top">
          <div className="container">
            <div className="navbar-header">
              <button type="button" className="navbar-toggle collapsed" data-toggle="collapse" data-target="#navbar" aria-expanded="false" aria-controls="navbar">
                <span className="sr-only">Toggle navigation</span>
                <span className="icon-bar" />
                <span className="icon-bar" />
                <span className="icon-bar" />
              </button>
              <a className="navbar-brand" href="/">Isaac Ranks</a>
            </div>
            <div id="navbar" className="navbar-collapse collapse">
              <ul className="nav navbar-nav">
                <li><Link to="/rebirth/vote">Vote (Rebirth)</Link></li>
                <li><Link to="/rebirth/ranks">Ranks (Rebirth)</Link></li>
                <li><a href="/donate">Donate</a></li>
                <li><a href="/changes">News</a></li>
              </ul>
            </div>
          </div>
        </nav>
        <div className="container">
          {this.props.children}
          <footer>
            Website © 2014-2015
            <a href="mailto:mithrandi@mithrandi.net">Tristan Seligmann</a>
            — I do not hold the copyright to any content from
            <a href="http://bindingofisaac.com/">The Binding of Isaac: Rebirth</a>
            or The Binding of Isaac: Afterbirth
            — Special thanks to
            <a href="https://www.reddit.com/r/bindingofisaac/">/r/bindingofisaac</a>
            and
            <a href="http://platinumgod.co.uk/">platinumgod.co.uk</a>a>
          </footer>
        </div>
      </div>
      )
  }
}

App.propTypes =
{ children: PropTypes.node
}

export default connect(state => ({routerState: state.router}))(App)
