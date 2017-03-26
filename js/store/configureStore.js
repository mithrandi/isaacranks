import {createStore, applyMiddleware} from 'redux'
import thunkMiddleware from 'redux-thunk'
import {routerMiddleware} from 'react-router-redux'

import rootReducer from '../reducers'
import fetchMiddleware from '../middleware/fetch'

export default function configureStore(history) {
    return createStore(
        rootReducer,
        applyMiddleware(
            thunkMiddleware,
            fetchMiddleware,
            routerMiddleware(history)))
}
