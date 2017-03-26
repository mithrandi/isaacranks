import React, {PropTypes} from 'react'
import {connect} from 'react-redux'
import {Navbar, Nav} from 'react-bootstrap'
import {Link} from 'react-router-dom'
import classnames from 'classnames'

const NavItem = ({className, active, disabled, style, children}) => {
    const classNames = classnames(className, { active, disabled })
    return (
        <li role="presentation" style={style} className={classNames}>
          {children}
        </li>
    )
}

@connect(state => ({routerState: state.router}))
export default class App extends React.Component {
    static propTypes =
        { children: PropTypes.node
        };

    render() {
        return (
            <div>
              <Navbar inverse fixedTop>
                <Navbar.Header>
                  <Navbar.Brand><a href="/">Isaac Ranks</a></Navbar.Brand>
                  <Navbar.Toggle />
                </Navbar.Header>
                <Navbar.Collapse>
                  <Nav navbar>
                    <NavItem eventKey={0}><Link to="/afterbirthplus/vote">Vote (Afterbirth+)</Link></NavItem>
                    <NavItem eventKey={1}><Link to="/afterbirthplus/ranks">Ranks (Afterbirth+)</Link></NavItem>
                    <NavItem eventKey={2}><Link to="/afterbirth/vote">Vote (Afterbirth)</Link></NavItem>
                    <NavItem eventKey={3}><Link to="/afterbirth/ranks">Ranks (Afterbirth)</Link></NavItem>
                    <NavItem eventKey={4}><Link to="/rebirth/vote">Vote (Rebirth)</Link></NavItem>
                    <NavItem eventKey={5}><Link to="/rebirth/ranks">Ranks (Rebirth)</Link></NavItem>
                    <NavItem eventKey={6}><Link to="/donate">Donate</Link></NavItem>
                    <NavItem eventKey={7}><Link to="/changes">News</Link></NavItem>
                  </Nav>
                </Navbar.Collapse>
              </Navbar>
              <div className="container">
                {this.props.children}
                <footer>
                  Website © 2014-2017 <a href="mailto:mithrandi@mithrandi.net">Tristan Seligmann</a> — I do not hold the copyright to any content from <a href="http://bindingofisaac.com/">The Binding of Isaac: Rebirth</a> or The Binding of Isaac: Afterbirth — Special thanks to <a href="https://www.reddit.com/r/bindingofisaac/">/r/bindingofisaac</a> and <a href="http://platinumgod.co.uk/">platinumgod.co.uk</a> — Come hang out in <a href="https://www.reddit.com/r/isaacranks">/r/isaacranks</a>!
                </footer>
              </div>
            </div>
        )
    }
}
