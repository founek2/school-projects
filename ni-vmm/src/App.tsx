import { SnackbarProvider } from 'notistack';
import React from 'react';
import { Provider } from 'react-redux';
import { BrowserRouter, Route, Routes } from 'react-router-dom';
import { PersistGate } from 'redux-persist/integration/react';
import { NotificationReduxConnect } from './components/NotificationReduxConnect';
import { persistor, store } from './store';
import { Home } from './views/Home';
import { AdapterDateFns } from '@mui/x-date-pickers/AdapterDateFns';
import { LocalizationProvider } from '@mui/x-date-pickers';

function App() {
    return (
        <Provider store={store}>
            <PersistGate persistor={persistor}>
                <SnackbarProvider maxSnack={3}>
                    <NotificationReduxConnect />
                    <LocalizationProvider dateAdapter={AdapterDateFns}>
                        <Home />
                    </LocalizationProvider>
                </SnackbarProvider>
            </PersistGate>
        </Provider>
    );
}

export default App;
