import { Box, Container, Grid, Rating, Slider, TextField, Typography } from '@mui/material';
import React from 'react';
import { WeightSlider } from '../../../components/WeightSlider';
import { useAppDispatch, useAppSelector } from '../../../hooks';
import { getPhotos, getRankingValue, getRankingWeight } from '../../../selectors/getters';
import { rankingActions } from '../../../store/slices/ranking';

const PROPERTY = 'views';
export function Views() {
    const weight = useAppSelector(getRankingWeight(PROPERTY));
    const value = useAppSelector(getRankingValue(PROPERTY));
    const dispatch = useAppDispatch();
    const photos = useAppSelector(getPhotos);

    return (
        <Grid container>
            <Grid item xs={12}>
                <Typography>Views</Typography>
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
                <Slider
                    sx={{ width: 200 }}
                    value={value}
                    max={photos.views.max}
                    min={photos.views.min}
                    size="small"
                    marks
                    valueLabelDisplay="auto"
                    step={1}
                    onChange={(e, v) => {
                        console.log('value', v);
                        dispatch(rankingActions.set({ [PROPERTY]: { value: v as number, weight } }));
                    }}
                />
            </Grid>
        </Grid>
    );
}
