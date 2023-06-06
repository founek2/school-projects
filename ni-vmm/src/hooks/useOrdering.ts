import { useAppSelector } from '.';
import { getRanking } from '../selectors/getters';
import { Photos, PhotoStore } from '../store/slices/photosSlice';
import ranking, { Ranking } from '../store/slices/ranking';
import { getDistance } from 'geolib';
import { differenceInSeconds, parse } from 'date-fns';
import { useMemo } from 'react';
import { levenshteinDistanceBetter } from '../utils/levenshteinDistance';
import { getDistanceFromLatLonInKm } from '../utils/haversineFormula';

function calcDistanceDate(targetDate: Date | string | number) {
    return (photo: PhotoStore) => {
        const taken = new Date(photo.datetaken);
        return Math.abs(differenceInSeconds(new Date(targetDate), taken));
    };
}
function calcDistanceWidth(target: number) {
    return (photo: PhotoStore) => {
        return photo.width_o ? Math.abs(target - photo.width_o) : 10000;
    };
}

function calcDistanceViews(target: number) {
    return (photo: PhotoStore) => {
        return photo.views ? Math.abs(target - photo.views) : 10000;
    };
}

// function calcDistanceOwnerName(target: string) {
//     return (photo: PhotoStore) => lavenshteinDistance(target, photo.ownername);
// }

function calcDistanceTag(target: string) {
    return (photo: PhotoStore) => {
        if (photo.tags.length === 0) return 100;

        return Math.min(...photo.tags.map((tag) => levenshteinDistanceBetter(target, tag)));
    };
}
type Point = { lng: number; lat: number };
function calcDistanceLocation(latLngA: Point) {
    return (photo: PhotoStore) =>
        getDistanceFromLatLonInKm(
            { lat: latLngA.lat, lng: latLngA.lng },
            { lat: photo.latitude, lng: photo.longitude }
        );
}

function normalizeAndInvert(data: number[]) {
    const maxValue = Math.max(...data);
    if (maxValue === 0) return data;

    return data.map((v) => 1 - v / maxValue);
}
function normalizeAndWeight(data: number[], weight: number = 0) {
    return normalizeAndInvert(data).map((v) => v * weight);
}

function sumAll(...obj: [ranking: { weight: number }, normDistances: number[]][]) {
    const lists = obj.map(([ranking, normDistances]) => {
        return normDistances.map((d) => d * ranking.weight);
    });
    return lists[0].map((v, i) => lists.reduce((acc, array, arrIdx) => acc + array[i], 0));
}
function calcWeightedDistances<T>(
    ranking: { weight?: number; value?: T },
    data: Photos['data'],
    distanceFn: (value: T) => (photo: PhotoStore) => number
) {
    const distances = ranking.value ? data.map(distanceFn(ranking.value)) : data.map(() => 0);

    const normalizedDistances = normalizeAndInvert(distances);
    return normalizedDistances;
}

function reorder(swapIndexes: { origIndex: number }[], data: any[]) {
    const copy = new Array(data.length);
    swapIndexes.forEach((swapBy, idx) => {
        // const [source, target] = [copy[swapBy.origIndex], copy[idx]];
        // copy[swapBy.origIndex] = target;
        copy[idx] = data[swapBy.origIndex];
    });

    return copy;
}

export function useOrdering(data: Photos['data']) {
    const location = useAppSelector(getRanking('location'));
    const dateTaken = useAppSelector(getRanking('dateTaken'));
    const width = useAppSelector(getRanking('width'));
    const tag = useAppSelector(getRanking('tag'));
    const views = useAppSelector(getRanking('views'));

    const ordered = useMemo((): [Photos['data'], Record<Exclude<keyof Ranking, 'disabled'>, number[]>, number[]] => {
        // const locationDistances = location.value ? data.map(calcDistanceLocation(location.value)) : data.map(() => 0);
        console.time('location');
        const locationDistances = calcWeightedDistances(location, data, calcDistanceLocation);
        console.timeEnd('location');

        console.time('width');
        const widthDistances = calcWeightedDistances(width, data, calcDistanceWidth);
        console.timeEnd('width');

        console.time('dateTaken');
        const dateTakenDistances = calcWeightedDistances(dateTaken, data, calcDistanceDate);
        console.timeEnd('dateTaken');

        console.time('tag');
        const tagDistances = calcWeightedDistances(tag, data, calcDistanceTag);
        console.timeEnd('tag');

        console.time('views');
        const viewsDistances = calcWeightedDistances(views, data, calcDistanceViews);
        console.timeEnd('views');

        console.time('final');
        const oneDistance = sumAll(
            [location, locationDistances],
            [dateTaken, dateTakenDistances],
            [width, widthDistances],
            [tag, tagDistances],
            [views, viewsDistances]
        );
        console.timeEnd('final');

        // console.log('distance tag', oneDistance);

        const swapIndexes = oneDistance
            // get photo id for each distance
            .map((v, idx) => ({ distance: v, origIndex: idx }))
            // sort by final distance
            .sort((a, b) => b.distance - a.distance);

        return [
            reorder(swapIndexes, data),
            {
                location: reorder(swapIndexes, locationDistances),
                width: reorder(swapIndexes, widthDistances),
                dateTaken: reorder(swapIndexes, dateTakenDistances),
                tag: reorder(swapIndexes, tagDistances),
                views: reorder(swapIndexes, viewsDistances),
            },
            reorder(swapIndexes, oneDistance),
        ];
    }, [data, location, dateTaken, width, tag, views]);

    return ordered;
}
