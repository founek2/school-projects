import { RootState } from '../store';

export const getNotifications = (state: RootState) => state.notifications;
export const getPhotos = (state: RootState) => state.photos;
export const getTags = (state: RootState) => state.photos.tags;
export const getRanking =
    <T extends keyof RootState['ranking']>(key: T) =>
    (state: RootState) =>
        state.ranking[key];

export const getRankingWeight =
    <T extends keyof RootState['ranking']>(key: T) =>
    (state: RootState) =>
        state.ranking[key].weight;
export const getRankingValue =
    <T extends keyof RootState['ranking']>(key: T) =>
    (state: RootState): RootState['ranking'][T]['value'] =>
        state.ranking[key].value;
