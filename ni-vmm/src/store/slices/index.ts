import { ActionFromReducersMapObject, combineReducers, Reducer, StateFromReducersMapObject } from '@reduxjs/toolkit';
import { api } from '../../api';
// import application from './application';
// import formsData from './formDataSlice';
import { persistReducer } from 'redux-persist';
import storage from 'redux-persist/lib/storage'; // defaults to localStorage for web
import notifications from './notificationSlice';
import photos from './photosSlice';
import ranking from './ranking';
// import preferences from './preferences';

const reducers = {
    [api.reducerPath]: api.reducer,
    notifications,
    photos,
    ranking,
};

type ReducersMapObject = typeof reducers;

const rootReducer = combineReducers(reducers);

const persistConfig = {
    key: 'application',
    storage,
    blacklist: [api.reducerPath],
};

export default persistReducer(persistConfig, rootReducer) as unknown as Reducer<
    StateFromReducersMapObject<ReducersMapObject>,
    ActionFromReducersMapObject<ReducersMapObject>
>;
