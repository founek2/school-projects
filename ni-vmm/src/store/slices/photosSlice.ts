import { createSlice } from '@reduxjs/toolkit';
import { Photo, searchApi } from '../../api/search';

export interface PhotoStore {
    id: string;
    owner: string;

    secret: string;
    server: string;
    farm: 66;
    title: string;
    ispublic: 1 | 0;
    isfriend: 1 | 0;
    isfamily: 1 | 0;
    url_c: string;
    url_o: string;
    url_sq: string;

    latitude: number;
    longitude: number;
    tags: string[]; // space separated list
    width_o?: number; // width of the original
    datetaken: string;
    ownername: string;
    views: number;
}
export type Photos = {
    data: PhotoStore[];
    tags: string[];
    views: { min: number; max: number };
    pages: number;
    perPage: number;
    total: number;
};
// Define the initial state using that type
const initialState: Photos = { data: [], tags: [], views: { min: 0, max: 0 }, pages: 0, perPage: 0, total: 0 };

function fallbackZero(num?: number) {
    if (num && Number.isFinite(num)) return num;

    return 0;
}

function convertToStorePhoto(photo: Photo) {
    return {
        ...photo,
        latitude: Number(photo.latitude),
        longitude: Number(photo.longitude),
        tags: photo.tags.split(' ').filter(Boolean),
        views: Number(photo.views),
    };
}

export const photosSlice = createSlice({
    name: 'photos',
    // `createSlice` will infer the state type from the `initialState` argument
    initialState,
    reducers: {
        // setAll: (state, action: PayloadAction<Photo[]>) => {
        //     return action.payload.map(convertToStorePhoto);
        // },
        // remove: (state, action: PayloadAction<PhotoStore['id']>) => {
        //     return state.filter((photo) => photo.id != action.payload);
        // },
        // add: (state, action: PayloadAction<Photo>) => {
        //     state.push(convertToStorePhoto(action.payload));
        // },
    },
    extraReducers: (builder) => {
        builder.addMatcher(searchApi.endpoints.photos.matchFulfilled, (state, { payload }): Photos => {
            const data = payload.photo.map(convertToStorePhoto);
            const tags = new Set<string>();
            data.map((p) => p.tags)
                .flat()
                .forEach((tag) => tags.add(tag));
            const views = data.map((p) => p.views);

            return {
                data,
                views: { min: fallbackZero(Math.min(...views)), max: fallbackZero(Math.max(...views)) },
                tags: [...tags],
                perPage: payload.perpage,
                pages: parseInt(payload.pages),
                total: parseInt(payload.total),
            };
        });
    },
});

export const photosActions = photosSlice.actions;

export default photosSlice.reducer;
