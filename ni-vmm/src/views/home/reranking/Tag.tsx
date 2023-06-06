import { Autocomplete, Container, Grid, Rating, TextField, Typography } from '@mui/material';
import React from 'react';
import { WeightSlider } from '../../../components/WeightSlider';
import { useAppDispatch, useAppSelector } from '../../../hooks';
import { getRankingValue, getRankingWeight, getTags } from '../../../selectors/getters';
import { rankingActions } from '../../../store/slices/ranking';

const PROPERTY = 'tag';
export function Tag() {
    const weight = useAppSelector(getRankingWeight(PROPERTY));
    const value = useAppSelector(getRankingValue(PROPERTY));
    const dispatch = useAppDispatch();
    const tags = useAppSelector(getTags);

    return (
        <Grid container>
            <Grid item xs={12}>
                <Typography>Tag</Typography>
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
                <Autocomplete
                    value={value || ''}
                    options={tags}
                    renderInput={(params) => <TextField {...params} sx={{ width: 230 }} />}
                    onChange={(e, v) => {
                        dispatch(rankingActions.set({ [PROPERTY]: { weight, value: v! } }));
                    }}
                />
            </Grid>
        </Grid>
    );
}
