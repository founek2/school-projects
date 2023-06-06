import { Box, Button, Paper, Rating, Typography } from '@mui/material';
import React, { useState } from 'react';
import { useAppDispatch, useAppSelector } from '../../hooks';
import { getRanking, getRankingValue } from '../../selectors/getters';
import { Ranking, rankingActions } from '../../store/slices/ranking';
import { Date } from './reranking/Date';
import { Location } from './reranking/Location';
import { MinMax } from './reranking/MinMax';
import { Tag } from './reranking/Tag';
import { Views } from './reranking/Views';

export function Reranking() {
    const dispatch = useAppDispatch();
    const disabled = useAppSelector(getRankingValue('disabled'));

    return (
        <Paper sx={{ padding: 3, paddingRight: 1, paddingTop: 1 }}>
            <Box sx={{ opacity: disabled ? 0.3 : 1 }}>
                <Typography variant="h5">Reranking</Typography>
                <Location />
                <Date />
                <MinMax property="width" title="Width (px)" />
                <Tag />
                <Views />
            </Box>

            <Button onClick={() => dispatch(rankingActions.reset())}>Reset</Button>
            <Button onClick={() => dispatch(rankingActions.set({ disabled: { weight: 0, value: !disabled } }))}>
                {disabled ? 'Enable' : 'Disable'}
            </Button>
        </Paper>
    );
}
