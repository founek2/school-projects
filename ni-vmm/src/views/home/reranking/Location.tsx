import { Container, Grid, Rating, Slider, Typography } from '@mui/material';
import React, { useMemo } from 'react';
import { LocationPicker } from '../../../components/LocationPicker';
import { WeightSlider } from '../../../components/WeightSlider';
import { useAppDispatch, useAppSelector } from '../../../hooks';
import { getPhotos, getRankingValue, getRankingWeight } from '../../../selectors/getters';
import { rankingActions } from '../../../store/slices/ranking';

const PROPERTY = 'location';
export function Location() {
    const weight = useAppSelector(getRankingWeight(PROPERTY));
    const value = useAppSelector(getRankingValue(PROPERTY));
    const dispatch = useAppDispatch();
    const photos = useAppSelector(getPhotos);

    const locations = useMemo(
        () =>
            photos.data.map((photo) => ({
                lat: photo.latitude,
                lng: photo.longitude,
                id: photo.id,
                icon: photo.url_sq,
            })),
        [photos]
    );

    return (
        <Grid container>
            <Grid item xs={12}>
                <Typography>GPS location</Typography>
            </Grid>

            <Grid item sx={{ paddingLeft: 4 }}>
                <WeightSlider
                    value={weight}
                    onChange={(e, newValue) => {
                        dispatch(rankingActions.set({ [PROPERTY]: { weight: newValue ?? 0, value } }));
                    }}
                />
            </Grid>
            <Grid item>
                <LocationPicker
                    value={value}
                    onChange={(e) => {
                        dispatch(rankingActions.set({ [PROPERTY]: { weight, value: e.target.value } }));
                    }}
                    markers={locations}
                />
            </Grid>
        </Grid>
    );
}
