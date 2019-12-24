import React, {PropTypes} from 'react'
import {connect} from 'react-redux'
import {Navbar, Nav, NavItem, NavDropdown} from 'react-bootstrap'
//import {LinkContainer} from 'react-router-bootstrap'
import LinkContainer from '../components/LinkContainer'


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
                    <NavDropdown eventKey="0" title="Vote" id="nav-dropdown-vote">
                      <LinkContainer to="/afterbirthplus/vote"><NavItem eventKey="0.1">Afterbirth+</NavItem></LinkContainer>
                      <LinkContainer to="/afterbirth/vote"><NavItem eventKey="0.2">Afterbirth</NavItem></LinkContainer>
                      <LinkContainer to="/rebirth/vote"><NavItem eventKey="0.3">Rebirth</NavItem></LinkContainer>
                    </NavDropdown>
                    <NavDropdown eventKey="1" title="Ranks" id="nav-dropdown-ranks">
                      <LinkContainer to="/afterbirthplus/ranks"><NavItem eventKey="1.1">Afterbirth+</NavItem></LinkContainer>
                      <LinkContainer to="/afterbirth/ranks"><NavItem eventKey="1.2">Afterbirth</NavItem></LinkContainer>
                      <LinkContainer to="/rebirth/ranks"><NavItem eventKey="1.3">Rebirth</NavItem></LinkContainer>
                    </NavDropdown>
                    <LinkContainer to="/donate"><NavItem eventKey="2">Donate</NavItem></LinkContainer>
                    <LinkContainer to="/changes"><NavItem eventKey="3">News</NavItem></LinkContainer>
                  </Nav>
                </Navbar.Collapse>
              </Navbar>
              <div className="container">
                {this.props.children}
                <footer>
                  Website © 2014 <a href="mailto:mithrandi@mithrandi.net">Tristan Seligmann</a> — I do not hold the copyright to any content from <a href="http://bindingofisaac.com/">The Binding of Isaac</a> — Special thanks to <a href="https://www.reddit.com/r/bindingofisaac/">/r/bindingofisaac</a> and <a href="http://platinumgod.co.uk/">platinumgod.co.uk</a> — Come hang out in <a href="https://www.reddit.com/r/isaacranks">/r/isaacranks</a>!
                </footer>
              </div>
            </div>
        )
    }
}
