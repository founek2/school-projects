import { Container, Grid, Rating, TextField, Typography } from '@mui/material';
import React from 'react';
import { WeightSlider } from '../../../components/WeightSlider';
import { useAppDispatch, useAppSelector } from '../../../hooks';
import { getRankingValue, getRankingWeight } from '../../../selectors/getters';
import { rankingActions } from '../../../store/slices/ranking';

type NumberProps = {
    title: string;
    property: 'width';
};

export function Number({ property, title }: NumberProps) {
    const weight = useAppSelector(getRankingWeight(property));
    const value = useAppSelector(getRankingValue(property));
    const dispatch = useAppDispatch();

    return (
        <Grid container>
            <Grid item xs={12}>
                <Typography>{title}</Typography>
            </Grid>
            <Grid item sx={{ paddingLeft: 4 }}>
                <WeightSlider
                    value={weight}
                    onChange={(e, newValue) => {
                        dispatch(rankingActions.set({ [property]: { weight: newValue ?? 0, value } }));
                    }}
                />
            </Grid>
            <Grid item>
                <TextField
                    type="number"
                    value={value || 0}
                    sx={{ width: 230 }}
                    onChange={(e) => {
                        dispatch(rankingActions.set({ [property]: { weight, value: window.Number(e.target.value) } }));
                    }}
                />
            </Grid>
        </Grid>
    );
}
