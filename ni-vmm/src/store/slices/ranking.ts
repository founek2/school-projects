import { createSlice, PayloadAction } from '@reduxjs/toolkit';

export type Ranking = {
    location: {
        weight: number;
        value?: {
            lat: number;
            lng: number;
        };
    };
    dateTaken: { weight: number; value?: string };
    tag: { weight: number; value?: string };
    width: { weight: number; value?: number };
    views: { weight: number; value?: number };
    disabled: { weight: number; value: boolean };
};
// Define the initial state using that type
const initialState: Ranking = {
    location: { weight: 0 },
    dateTaken: { weight: 0 },
    tag: { weight: 0 },
    width: { weight: 0 },
    views: { weight: 0 },
    disabled: { weight: 0, value: false },
};

export const rankingSlice = createSlice({
    name: 'ranking',
    // `createSlice` will infer the state type from the `initialState` argument
    initialState,
    reducers: {
        set: (state, action: PayloadAction<Partial<Ranking>>) => {
            Object.entries(action.payload).forEach(([k, value]) => {
                // @ts-ignore
                state[k] = value;
            });
        },
        reset: (state, action: PayloadAction) => {
            return initialState;
        },
        // add: (state, action: PayloadAction<Photo>) => {
        //     state.push(convertToStorePhoto(action.payload));
        // },
    },
});

export const rankingActions = rankingSlice.actions;

export default rankingSlice.reducer;
