import { Button, Container, Grid, Rating, TextField, Typography } from '@mui/material';
import React, { useState } from 'react';
import { WeightSlider } from '../../../components/WeightSlider';
import { useAppDispatch, useAppSelector } from '../../../hooks';
import { getPhotos, getRankingValue, getRankingWeight } from '../../../selectors/getters';
import { rankingActions } from '../../../store/slices/ranking';
import ArrowDropDownIcon from '@mui/icons-material/ArrowDropDown';
import ArrowDropUpIcon from '@mui/icons-material/ArrowDropUp';

type NumberProps = {
    title: string;
    property: 'width';
};

export function MinMax({ property, title }: NumberProps) {
    const [order, setOrder] = useState<'desc' | 'asc'>('desc');
    const photos = useAppSelector(getPhotos);
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
                <Button
                    endIcon={order === 'desc' ? <ArrowDropDownIcon /> : <ArrowDropUpIcon />}
                    onClick={(e) => {
                        const newValue =
                            order === 'asc'
                                ? photos.data.reduce((acc, p) => (acc > (p.width_o || 0) ? acc : p.width_o || acc), 0)
                                : photos.data.reduce(
                                      (acc, p) => (acc < (p.width_o || 0) || 0 ? acc : p.width_o || acc),
                                      Number.MAX_SAFE_INTEGER
                                  );

                        dispatch(rankingActions.set({ [property]: { weight, value: newValue } }));
                        setOrder(order === 'desc' ? 'asc' : 'desc');
                    }}
                >
                    {order === 'desc' ? 'Max target' : 'Min target'}
                </Button>
            </Grid>
        </Grid>
    );
}
