import { Container, Grid, Rating, TextField, Typography } from '@mui/material';
import { DesktopDatePicker } from '@mui/x-date-pickers/DesktopDatePicker';
import React from 'react';
import { WeightSlider } from '../../../components/WeightSlider';
import { useAppDispatch, useAppSelector } from '../../../hooks';
import { getRankingValue, getRankingWeight } from '../../../selectors/getters';
import { rankingActions } from '../../../store/slices/ranking';

const PROPERTY = 'dateTaken';
export function Date() {
    const weight = useAppSelector(getRankingWeight(PROPERTY));
    const value = useAppSelector(getRankingValue(PROPERTY));
    const dispatch = useAppDispatch();

    return (
        <Grid container>
            <Grid item xs={12}>
                <Typography>Date of capture</Typography>
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
                <DesktopDatePicker
                    inputFormat="dd. MM. yyyy"
                    value={value || null}
                    // sx={{borderolor}}
                    onChange={(value) => {
                        if (value) dispatch(rankingActions.set({ [PROPERTY]: { weight, value: value.toString() } }));
                    }}
                    renderInput={(params) => <TextField {...params} />}
                />
            </Grid>
        </Grid>
    );
}
