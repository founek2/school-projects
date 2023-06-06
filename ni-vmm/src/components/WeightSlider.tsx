import { Box, Slider } from '@mui/material';
import React from 'react';

export interface WeightSliderProps {
    value: number;
    onChange: (e: Event, value: number) => any;
}
export function WeightSlider({ value, onChange }: WeightSliderProps) {
    return (
        <Box sx={{ mr: 4 }}>
            <Slider
                sx={{ width: 200 }}
                value={value * 10}
                max={10}
                size="small"
                marks
                valueLabelDisplay="auto"
                step={0.5}
                onChange={(e, v) => onChange(e, (v as number) / 10)}
            />
        </Box>
    );
}
